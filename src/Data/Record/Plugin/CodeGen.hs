{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

module Data.Record.Plugin.CodeGen (genLargeRecord) where

import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.GHC
import Data.Record.Plugin.RuntimeNames as Runtime
import Data.Record.Plugin.Types

genLargeRecord :: Record -> [LHsDecl GhcPs]
genLargeRecord rec =
  concat
    [ [genDatatype rec],
      genVectorFrom rec,
      genVectorTo rec,
      genUnsafeGetIndex rec,
      genUnsafeSetIndex rec,
      getHasFieldInstances rec,
      genMeta rec,
      genStockInstances rec
    ]

genDatatype :: Record -> LHsDecl GhcPs
genDatatype Record {tyName, conName, tyVars, fields, derivings} =
  noLoc
    ( TyClD
        NoExt
        DataDecl
          { tcdDExt = NoExt,
            tcdLName = noLoc tyName,
            tcdTyVars = mkHsQTvs (noLoc <$> tyVars),
            tcdFixity = Prefix,
            tcdDataDefn =
              HsDataDefn
                { dd_ext = NoExt,
                  dd_ND = DataType,
                  dd_ctxt = noLoc [],
                  dd_cType = Nothing,
                  dd_kindSig = Nothing,
                  dd_cons =
                    [ noLoc
                        ConDeclH98
                          { con_ext = NoExt,
                            con_name = noLoc conName,
                            con_forall = noLoc True,
                            con_ex_tvs = [noLoc (UserTyVar NoExt (noLoc v)) | v <- vars],
                            con_mb_cxt = Just (noLoc [mkEqConstr v t | v <- vars | (_, t) <- fields]),
                            con_args = RecCon (noLoc [mkRecField f v | v <- vars | (f, _) <- fields]),
                            con_doc = Nothing
                          }
                    ],
                  dd_derivs = noLoc [anyclassDeriving c | DeriveAnyClass c <- derivings]
                }
          }
    )
  where
    vars = [tyVarRdr ("lr_f" <> show i) | (i, _) <- zip [1 ..] fields]

    mkEqConstr var ty = opT (varT var) typeEq (noLoc ty)
    mkRecField field var = conDeclField field (bangT (varT var))

genVectorFrom :: Record -> [LHsDecl GhcPs]
genVectorFrom rec@Record {tyName, conName, tyVars, fields} =
  let body =
        lamE
          [conP conName [varP f | (f, _) <- fields]]
          (appE Runtime.vectorFromList (listE [appE Runtime.unsafeCoerce (varE f) | (f, _) <- fields]))
   in simpleFn (nameVectorFrom rec) (arrT (recordTy rec) (appT Runtime._Vector Runtime._Any)) body

genVectorTo :: Record -> [LHsDecl GhcPs]
genVectorTo rec@Record {tyName, conName, tyVars, fields} =
  let body =
        lamE [varP nameArg] do
          caseE
            (appE Runtime.vectorToList (varE nameArg))
            [ (listP [varP f | (f, _) <- fields], appsE (varE conName) [appE Runtime.unsafeCoerce (varE f) | (f, _) <- fields]),
              (wildP, appE Runtime.error (stringE matchErr))
            ]
   in simpleFn (nameVectorTo rec) (arrT (appT Runtime._Vector Runtime._Any) (recordTy rec)) body
  where
    nameArg = exprVarRdr "x"
    matchErr =
      concat
        [ "Pattern match failure in ",
          rdrNameString (nameVectorTo rec),
          ": vector with invalid number of elements."
        ]

genUnsafeGetIndex :: Record -> [LHsDecl GhcPs]
genUnsafeGetIndex rec =
  simpleFn
    (nameUnsafeGetIndex rec)
    (arrT Runtime._Int (arrT (recordTy rec) (varT (tyVarRdr "lr_result_"))))
    ( lamE
        [varP index, varP arg]
        (appE Runtime.noInlineUnsafeCo (appsE Runtime.vectorUnsafeIndex [appE (varE (nameVectorFrom rec)) (varE arg), varE index]))
    )
  where
    index = exprVarRdr "index"
    arg = exprVarRdr "arg"

genUnsafeSetIndex :: Record -> [LHsDecl GhcPs]
genUnsafeSetIndex rec =
  simpleFn
    (nameUnsafeSetIndex rec)
    (arrT Runtime._Int (arrT (recordTy rec) (arrT (varT (tyVarRdr "lr_result_")) (recordTy rec))))
    ( lamE
        [varP index, varP arg, bangP (varP val)]
        ( appE
            (varE (nameVectorTo rec))
            ( appsE
                Runtime.vectorUnsafeUpd
                [ appE (varE (nameVectorFrom rec)) (varE arg),
                  listE [tupleE [varE index, appE Runtime.noInlineUnsafeCo (varE val)]]
                ]
            )
        )
    )
  where
    index = exprVarRdr "index"
    arg = exprVarRdr "arg"
    val = exprVarRdr "val"

genHasFieldInstance :: Record -> Int -> (RdrName, HsType GhcPs) -> LHsDecl GhcPs
genHasFieldInstance rec index (fieldName, fieldTy) =
  instanceD_simple
    [opT (varT fieldTyVar) typeEq (noLoc fieldTy)]
    (appsT Runtime._HasField [stringT fieldStr, recordTy rec, varT fieldTyVar])
    [ simpleBind
        (exprVarRdr "hasField")
        ( lamE
            [varP arg]
            ( tupleE
                [ appsE (varE (nameUnsafeSetIndex rec)) [intE index, varE arg],
                  appsE (varE (nameUnsafeGetIndex rec)) [intE index, varE arg]
                ]
            )
        )
    ]
    []
  where
    fieldStr = rdrNameString fieldName
    arg = exprVarRdr "arg"
    fieldTyVar = tyVarRdr "lr_field_ty"

getHasFieldInstances :: Record -> [LHsDecl GhcPs]
getHasFieldInstances rec@Record {fields} =
  [genHasFieldInstance rec i f | i <- [0 ..] | f <- fields]

genConstraintsClass :: Record -> LHsDecl GhcPs
genConstraintsClass rec@Record {tyVars} =
  (classD_simple [] (nameConstraints rec) (map noLoc tyVars ++ [kindedTyVarBndr c (arrT Runtime._Type Runtime._Constraint)]))
    [ classOpSig (nameDictConstraints rec) (arrT (appT Runtime._Proxy (varT c)) (appsT Runtime._Rep [appT Runtime._Dict (varT c), recordTy rec]))
    ]
  where
    c = mkRdrUnqual (mkTyVarOcc "lr_con_c")

genConstraintsInstance :: Record -> LHsDecl GhcPs
genConstraintsInstance rec@Record {tyVars, fields} =
  instanceD_simple
    [appT (varT c) (noLoc ty) | (_, ty) <- fields]
    (appsT (varT (nameConstraints rec)) ([varT (hsTyVarName v) | v <- tyVars] ++ [varT c]))
    [simpleBind (nameDictConstraints rec) body]
    []
  where
    c = mkRdrUnqual (mkTyVarOcc "lr_con_c")
    p = mkRdrUnqual (mkVarOcc "p")
    body = lamE [varP p] (appE (varE Runtime._C_Rep) (appE Runtime.vectorFromList (listE dicts)))
    dicts = [mkDict (noLoc ty) | (_, ty) <- fields]

    mkDict :: LHsType GhcPs -> LHsExpr GhcPs
    mkDict ty = appE Runtime.noInlineUnsafeCo (appsE Runtime.dictFor [varE p, genProxy ty])

genGenericInstance :: Record -> LHsDecl GhcPs
genGenericInstance rec@Record {tyVars, conName, fields} =
  instanceD_simple
    []
    (appT Runtime._Generic (recordTy rec))
    [ simpleBind Runtime.from (lamE [varP x] (appE Runtime.repFromVector (appE (varE (nameVectorFrom rec)) (varE x)))),
      simpleBind Runtime.to (lamE [varP x] (appE (lamE [varP y] (appsE Runtime.seq [appE Runtime.rnfVectorAny (varE y), appE (varE (nameVectorTo rec)) (varE y)])) (appE Runtime.repToVector (varE x)))),
      simpleBind Runtime.dict (varE (nameDictConstraints rec)),
      simpleBind Runtime.metadata do
        lamE [varP x] do
          (recConE Runtime._C_Metadata)
            [ Runtime._F_recordName `recFieldE` stringE (recordName rec),
              Runtime._F_recordConstructor `recFieldE` stringE (rdrNameString conName),
              Runtime._F_recordSize `recFieldE` intE (length fields),
              Runtime._F_recordFieldMetadata `recFieldE` (appE (varE Runtime._C_Rep) (appE Runtime.vectorFromList metadata))
            ]
    ]
    [ tfInstanceD Runtime._Constraints [recordTy rec] constraints,
      tfInstanceD Runtime._MetadataOf [recordTy rec] metadataOf
    ]
  where
    (x, y) = (exprVarRdr "x", exprVarRdr "y")

    constraints = appsT (varT (nameConstraints rec)) [varT (hsTyVarName v) | v <- tyVars]
    metadataOf = listT [tupleT [stringT (rdrNameString f), noLoc t] | (f, t) <- fields]
    metadata = listE [mkFieldMD name | (name, _) <- fields]
    mkFieldMD name =
      (appsE (varE Runtime._C_FieldMetadata))
        [ genProxy (stringT (rdrNameString name)),
          varE Runtime._C_FieldStrict
        ]

genGHCGeneric :: Record -> LHsDecl GhcPs
genGHCGeneric rec =
  (instanceD_simple [] (appT Runtime._GHC_Generic (recordTy rec)))
    [ simpleBind Runtime._GHC_from (varE Runtime._C_WrapThroughLRGenerics),
      simpleBind Runtime._GHC_to Runtime.unwrapThroughLRGenerics
    ]
    [ tfInstanceD Runtime._GHC_Rep [recordTy rec] (appT Runtime._ThroughLRGenerics (recordTy rec))
    ]

genMeta :: Record -> [LHsDecl GhcPs]
genMeta rec =
  [ genConstraintsClass rec,
    genConstraintsInstance rec,
    genGenericInstance rec,
    genGHCGeneric rec
  ]

-- TODO: nub
genRequiredContext :: Record -> LHsType GhcPs -> [LHsType GhcPs]
genRequiredContext Record {fields} c =
  [appT c (noLoc t) | (_, t) <- fields, hasTypeVars t]
  where
    hasTypeVars :: HsType GhcPs -> Bool
    hasTypeVars = any (isTvOcc . rdrNameOcc) . Uniplate.universeBi

genStockInstances :: Record -> [LHsDecl GhcPs]
genStockInstances rec@Record {derivings} = [genStockInstance rec d | DeriveStock d <- derivings]

genStockInstance :: Record -> StockDeriving -> LHsDecl GhcPs
genStockInstance rec deriv =
  instanceD_simple
    (genRequiredContext rec cls)
    (appT cls (recordTy rec))
    [simpleBind mthd gen]
    []
  where
    (cls, mthd, gen) = case deriv of
      Show -> (Runtime._Show, Runtime.showsPrec, Runtime.gshowsPrec)
      Eq -> (Runtime._Eq, Runtime.eq, Runtime.geq)
      Ord -> (Runtime._Ord, Runtime.compare, Runtime.gcompare)

genProxy :: LHsType GhcPs -> LHsExpr GhcPs
genProxy ty = typeSigE (varE Runtime._C_Proxy) (appT Runtime._Proxy ty)

-- TODO: error message about fail

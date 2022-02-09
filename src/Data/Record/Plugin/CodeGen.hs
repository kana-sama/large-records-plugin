{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

module Data.Record.Plugin.CodeGen (genLargeRecord) where

import Control.Monad (when)
import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.GHC
import Data.Record.Plugin.RuntimeNames as Runtime
import Data.Record.Plugin.Types.Options (shouldGeneratedHasField, shouldRecordBeStrict)
import Data.Record.Plugin.Types.Record (Record (..), RecordDeriving (..), StockDeriving (..))

recordName :: Record -> String
recordName Record {tyName} = occNameString (rdrNameOcc tyName)

nameVectorFrom :: Record -> RdrName
nameVectorFrom rec = varRdr ("vectorFrom" <> recordName rec)

nameVectorTo :: Record -> RdrName
nameVectorTo rec = varRdr ("vectorTo" <> recordName rec)

nameUnsafeGetIndex :: Record -> RdrName
nameUnsafeGetIndex rec = varRdr ("unsafeGetIndex" <> recordName rec)

nameUnsafeSetIndex :: Record -> RdrName
nameUnsafeSetIndex rec = varRdr ("unsafeSetIndex" <> recordName rec)

nameConstraints :: Record -> RdrName
nameConstraints rec = mkRdrUnqual (mkTcOcc ("Constraints_" <> recordName rec))

nameDictConstraints :: Record -> RdrName
nameDictConstraints rec = varRdr ("dictConstraints_" <> recordName rec)

genLargeRecord :: Record -> [LHsDecl GhcPs]
genLargeRecord rec@Record {fields, options} =
  concat
    [ [genDatatype rec],
      genVectorFrom rec,
      genVectorTo rec,
      genUnsafeGetIndex rec,
      genUnsafeSetIndex rec,
      if shouldGeneratedHasField options
        then [genHasFieldInstance rec i f | i <- [0 ..] | f <- fields]
        else [],
      [ genConstraintsClass rec,
        genConstraintsInstance rec,
        genGenericInstance rec,
        genGHCGeneric rec
      ],
      genStockInstances rec
    ]

-- | Makes record type with type variables
genRecordTy :: Record -> LHsType GhcPs
genRecordTy Record {tyName, tyVars} = varT tyName `appsT` [varT (hsTyVarName f) | f <- tyVars]

genDatatype :: Record -> LHsDecl GhcPs
genDatatype Record {tyName, conName, tyVars, fields, derivings, options} =
  noLoc
    ( TyClD
        noExtField
        DataDecl
          { tcdDExt = noExtField,
            tcdLName = noLoc tyName,
            tcdTyVars = mkHsQTvs (noLoc <$> tyVars),
            tcdFixity = Prefix,
            tcdDataDefn =
              HsDataDefn
                { dd_ext = noExtField,
                  dd_ND = DataType,
                  dd_ctxt = noLoc [],
                  dd_cType = Nothing,
                  dd_kindSig = Nothing,
                  dd_cons =
                    [ noLoc
                        ConDeclH98
                          { con_ext = noExtField,
                            con_name = noLoc conName,
                            con_forall = noLoc True,
                            con_ex_tvs = [noLoc (UserTyVar noExtField (noLoc v)) | v <- vars],
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
    vars = [varRdrT ("lr_f" <> show i) | (i, _) <- zip [1 ..] fields]

    mkEqConstr var ty = opT (varT var) typeEq (noLoc ty)
    mkRecField field var = conDeclField field (optionalBang (varT var))
    optionalBang = if shouldRecordBeStrict options then bangT else id

genVectorFrom :: Record -> [LHsDecl GhcPs]
genVectorFrom rec@Record {tyName, conName, tyVars, fields} =
  let body =
        lamE
          [conP conName [varP f | (f, _) <- fields]]
          (varE Runtime.fromList `appE` listE [varE Runtime.unsafeCoerce `appE` varE f | (f, _) <- fields])
   in simpleFn (nameVectorFrom rec) (genRecordTy rec `arrT` (varT Runtime.type_Vector `appT` varT Runtime.type_Any)) body

genVectorTo :: Record -> [LHsDecl GhcPs]
genVectorTo rec@Record {tyName, conName, tyVars, fields} =
  let body =
        lamE [varP nameArg] do
          caseE
            (varE Runtime.toList `appE` varE nameArg)
            [ (listP [varP f | (f, _) <- fields], appsE (varE conName) [varE Runtime.unsafeCoerce `appE` varE f | (f, _) <- fields]),
              (wildP, varE Runtime.error `appE` stringE matchErr)
            ]
   in simpleFn (nameVectorTo rec) ((varT Runtime.type_Vector `appT` varT Runtime.type_Any) `arrT` genRecordTy rec) body
  where
    nameArg = varRdr "x"
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
    (varT Runtime.type_Int `arrT` (genRecordTy rec `arrT` varT (varRdrT "lr_result_")))
    ( lamE
        [varP index, varP arg]
        (varE Runtime.noInlineUnsafeCo `appE` varE Runtime.unsafeIndex `appsE` [varE (nameVectorFrom rec) `appE` varE arg, varE index])
    )
  where
    index = varRdr "index"
    arg = varRdr "arg"

genUnsafeSetIndex :: Record -> [LHsDecl GhcPs]
genUnsafeSetIndex rec =
  simpleFn
    (nameUnsafeSetIndex rec)
    (varT Runtime.type_Int `arrT` (genRecordTy rec `arrT` (varT (varRdrT "lr_result_") `arrT` genRecordTy rec)))
    ( lamE
        [varP index, varP arg, bangP (varP val)]
        ( varE (nameVectorTo rec)
            `appE` ( varE Runtime.unsafeUpd
                       `appsE` [ varE (nameVectorFrom rec) `appE` varE arg,
                                 listE [tupleE [varE index, varE Runtime.noInlineUnsafeCo `appE` varE val]]
                               ]
                   )
        )
    )
  where
    index = varRdr "index"
    arg = varRdr "arg"
    val = varRdr "val"

genHasFieldInstance :: Record -> Int -> (RdrName, HsType GhcPs) -> LHsDecl GhcPs
genHasFieldInstance rec index (fieldName, fieldTy) =
  instanceD_simple
    [opT (varT fieldTyVar) typeEq (noLoc fieldTy)]
    (varT Runtime.type_HasField `appsT` [stringT fieldStr, genRecordTy rec, varT fieldTyVar])
    [ simpleBind
        (varRdr "hasField")
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
    arg = varRdr "arg"
    fieldTyVar = varRdrT "lr_field_ty"

genConstraintsClass :: Record -> LHsDecl GhcPs
genConstraintsClass rec@Record {tyVars} =
  (classD_simple [] (nameConstraints rec) (map noLoc tyVars ++ [kindedTyVarBndr c (varT Runtime.type_Type `arrT` varT Runtime.type_Constraint)]))
    [ classOpSig (nameDictConstraints rec) ((varT Runtime.type_Proxy `appT` varT c) `arrT` (varT Runtime.type_Rep `appsT` [varT Runtime.type_Dict `appT` varT c, genRecordTy rec]))
    ]
  where
    c = mkRdrUnqual (mkTyVarOcc "lr_con_c")

genConstraintsInstance :: Record -> LHsDecl GhcPs
genConstraintsInstance rec@Record {tyVars, fields} =
  instanceD_simple
    [varT c `appT` noLoc ty | (_, ty) <- fields]
    (appsT (varT (nameConstraints rec)) ([varT (hsTyVarName v) | v <- tyVars] ++ [varT c]))
    [simpleBind (nameDictConstraints rec) body]
    []
  where
    c = mkRdrUnqual (mkTyVarOcc "lr_con_c")
    p = mkRdrUnqual (mkVarOcc "p")
    body = lamE [varP p] (varE Runtime.con_Rep `appE` (varE Runtime.fromList `appE` listE dicts))
    dicts = [mkDict (noLoc ty) | (_, ty) <- fields]

    mkDict :: LHsType GhcPs -> LHsExpr GhcPs
    mkDict ty = varE Runtime.noInlineUnsafeCo `appE` (varE Runtime.dictFor `appsE` [varE p, genProxy ty])

genGenericInstance :: Record -> LHsDecl GhcPs
genGenericInstance rec@Record {tyVars, conName, fields} =
  instanceD_simple
    []
    (varT Runtime.type_Generic `appT` genRecordTy rec)
    [ simpleBind Runtime.from_unqual (lamE [varP x] (varE Runtime.repFromVector `appE` (varE (nameVectorFrom rec) `appE` varE x))),
      simpleBind Runtime.to_unqual (lamE [varP x] (appE (lamE [varP y] (varE Runtime.seq `appsE` [varE Runtime.rnfVectorAny `appE` varE y, varE (nameVectorTo rec) `appE` varE y])) (varE Runtime.repToVector `appE` varE x))),
      simpleBind Runtime.dict_unqual (varE (nameDictConstraints rec)),
      simpleBind Runtime.metadata_unqual do
        lamE [varP x] do
          (recConE Runtime.con_Metadata)
            [ Runtime.field_recordName `recFieldE` stringE (recordName rec),
              Runtime.field_recordConstructor `recFieldE` stringE (rdrNameString conName),
              Runtime.field_recordSize `recFieldE` intE (length fields),
              Runtime.field_recordFieldMetadata `recFieldE` (varE Runtime.con_Rep `appE` (varE Runtime.fromList `appE` metadata))
            ]
    ]
    [ tfInstanceD Runtime.type_Constraints_unqual [genRecordTy rec] constraints,
      tfInstanceD Runtime.type_MetadataOf_unqual [genRecordTy rec] metadataOf
    ]
  where
    (x, y) = (varRdr "x", varRdr "y")

    constraints = appsT (varT (nameConstraints rec)) [varT (hsTyVarName v) | v <- tyVars]
    metadataOf = listT [tupleT [stringT (rdrNameString f), noLoc t] | (f, t) <- fields]
    metadata = listE [mkFieldMD name | (name, _) <- fields]
    mkFieldMD name =
      (appsE (varE Runtime.con_FieldMetadata))
        [ genProxy (stringT (rdrNameString name)),
          varE Runtime.con_FieldStrict
        ]

genGHCGeneric :: Record -> LHsDecl GhcPs
genGHCGeneric rec =
  (instanceD_simple [] (varT Runtime.type_GHC_Generic `appT` genRecordTy rec))
    [ simpleBind Runtime._GHC_from_unqual (varE Runtime.con_WrapThroughLRGenerics),
      simpleBind Runtime._GHC_to_unqual (varE Runtime.unwrapThroughLRGenerics)
    ]
    [ tfInstanceD Runtime.type_GHC_Rep_unqual [genRecordTy rec] (varT Runtime.type_ThroughLRGenerics `appT` genRecordTy rec)
    ]

-- TODO: nub
genRequiredContext :: Record -> LHsType GhcPs -> [LHsType GhcPs]
genRequiredContext Record {fields} c =
  [c `appT` noLoc t | (_, t) <- fields, hasTypeVars t]
  where
    hasTypeVars :: HsType GhcPs -> Bool
    hasTypeVars = any (isTvOcc . rdrNameOcc) . Uniplate.universeBi

genStockInstances :: Record -> [LHsDecl GhcPs]
genStockInstances rec@Record {derivings} = do
  DeriveStock d <- derivings
  case genStockInstance rec d of
    Just decl -> pure decl
    Nothing -> []

-- | For each record type `R` and class name `C`, generate
--
-- > instance $(genRequiredContext R C) => C R where
-- >   $(method) = $(generic implementation)
--
-- Note: Generic deriving is ignored, because it always generates
genStockInstance :: Record -> StockDeriving -> Maybe (LHsDecl GhcPs)
genStockInstance rec = \case
  Show -> Just (mkInstance Runtime.type_Show Runtime.showsPrec (varE Runtime.gshowsPrec))
  Eq -> Just (mkInstance Runtime.type_Eq Runtime.eq (varE Runtime.geq))
  Ord -> Just (mkInstance Runtime.type_Ord Runtime.compare (varE Runtime.gcompare))
  Generic -> Nothing
  where
    mkInstance cls mthd gen =
      instanceD_simple
        (genRequiredContext rec (varT cls))
        (varT cls `appT` genRecordTy rec)
        [simpleBind mthd gen]
        []

-- | Creates Proxy expression
--
-- @genProxy ty@ -> @Proxy :: Proxy [ty]@
genProxy :: LHsType GhcPs -> LHsExpr GhcPs
genProxy ty = typeSigE (varE Runtime.con_Proxy) (varT Runtime.type_Proxy `appT` ty)

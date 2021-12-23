{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Data.Record.Plugin.GHC
  ( module Data.Record.Plugin.GHC,
    module OccName,
    module RdrName,
    module GHC,
    module BasicTypes,
    module GhcPlugins,
    module HsSyn,
  )
where

import Bag (emptyBag, listToBag)
import BasicTypes
import BasicTypes (PromotionFlag (NotPromoted))
import Data.List (foldl')
import Data.String (fromString)
import GHC
import GhcPlugins (HsParsedModule (..), Plugin (..), defaultPlugin, purePlugin)
import HsSyn
import OccName
import RdrName
import TcEvidence (HsWrapper (WpHole))
import TysWiredIn (eqTyCon_RDR)

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

tyVarRdr :: String -> RdrName
tyVarRdr = mkRdrUnqual . mkTyVarOcc

exprVarRdr :: String -> RdrName
exprVarRdr = mkRdrUnqual . mkVarOcc

stringL :: String -> HsLit GhcPs
stringL = HsString NoSourceText . fromString

stringTL :: String -> HsTyLit
stringTL = HsStrTy NoSourceText . fromString

intL :: Integral a => a -> HsLit GhcPs
intL = HsInt NoExt . mkIntegralLit

litE :: HsLit GhcPs -> LHsExpr GhcPs
litE = noLoc . HsLit NoExt

litT :: HsTyLit -> LHsType GhcPs
litT = noLoc . HsTyLit NoExt

stringE :: String -> LHsExpr GhcPs
stringE = litE . stringL

stringT :: String -> LHsType GhcPs
stringT = litT . stringTL

intE :: Integral a => a -> LHsExpr GhcPs
intE = litE . intL

varT :: RdrName -> LHsType GhcPs
varT name = noLoc (HsTyVar NoExt NotPromoted (noLoc name))

varE :: RdrName -> LHsExpr GhcPs
varE name = noLoc (HsVar NoExt (noLoc name))

varP :: RdrName -> LPat GhcPs
varP name = noLoc (VarPat NoExt (noLoc name))

conP :: RdrName -> [LPat GhcPs] -> LPat GhcPs
conP con args = noLoc (ConPatIn (noLoc con) (PrefixCon args))

opT :: LHsType GhcPs -> RdrName -> LHsType GhcPs -> LHsType GhcPs
opT l op r = noLoc (mkHsOpTy l (noLoc op) r)

bangT :: LHsType GhcPs -> LHsType GhcPs
bangT = noLoc . HsBangTy NoExt (HsSrcBang NoSourceText NoSrcUnpack SrcStrict)

qualT :: [LHsType GhcPs] -> LHsType GhcPs -> LHsType GhcPs
qualT ctx a = noLoc (HsQualTy NoExt (noLoc ctx) a)

arrT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
arrT a b = noLoc (HsFunTy NoExt a b)

appT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
appT = mkHsAppTy

parE :: LHsExpr GhcPs -> LHsExpr GhcPs
parE = noLoc . HsPar NoExt

recFieldE :: RdrName -> LHsExpr GhcPs -> LHsRecField GhcPs (LHsExpr GhcPs)
recFieldE name val = noLoc (HsRecField (noLoc (mkFieldOcc (noLoc name))) val False)

recConE :: RdrName -> [LHsRecField GhcPs (LHsExpr GhcPs)] -> LHsExpr GhcPs
recConE name fields = noLoc (RecordCon NoExt (noLoc name) (HsRecFields fields Nothing))

bangP :: LPat GhcPs -> LPat GhcPs
bangP = noLoc . BangPat NoExt

appE :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
appE a b = mkHsApp a b

listE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
listE = noLoc . ExplicitList NoExt Nothing

listT :: [LHsType GhcPs] -> LHsType GhcPs
listT = noLoc . HsExplicitListTy NoExt IsPromoted

tupleE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
tupleE xs = noLoc (ExplicitTuple NoExt [noLoc (Present NoExt x) | x <- xs] Boxed)

tupleT :: [LHsType GhcPs] -> LHsType GhcPs
tupleT = noLoc . HsExplicitTupleTy NoExt

listP :: [LPat GhcPs] -> LPat GhcPs
listP xs = noLoc (ListPat NoExt xs)

appsT :: LHsType GhcPs -> [LHsType GhcPs] -> LHsType GhcPs
appsT = foldl' appT

appsE :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
appsE = foldl' appE

wildP :: LPat GhcPs
wildP = noLoc (WildPat NoExt)

kindedTyVarBndr :: RdrName -> LHsType GhcPs -> LHsTyVarBndr GhcPs
kindedTyVarBndr name ty = noLoc (KindedTyVar NoExt (noLoc name) ty)

caseE :: LHsExpr GhcPs -> [(LPat GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
caseE x alts =
  noLoc (HsCase NoExt x (MG NoExt (noLoc (map mkAlt alts)) Generated))
  where
    mkAlt :: (LPat GhcPs, LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
    mkAlt (pat, body) = noLoc (Match NoExt CaseAlt [pat] (simpleGHRSs body))

conDeclField :: RdrName -> LHsType GhcPs -> LConDeclField GhcPs
conDeclField name ty =
  noLoc
    ConDeclField
      { cd_fld_ext = NoExt,
        cd_fld_names = [noLoc (mkFieldOcc (noLoc name))],
        cd_fld_type = ty,
        cd_fld_doc = Nothing
      }

simpleGHRSs :: LHsExpr GhcPs -> GRHSs GhcPs (LHsExpr GhcPs)
simpleGHRSs body = GRHSs NoExt [noLoc (GRHS NoExt [] body)] (noLoc (EmptyLocalBinds NoExt))

lamE :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
lamE pats body = noLoc (HsLam NoExt (MG NoExt (noLoc [noLoc (Match NoExt LambdaExpr pats (simpleGHRSs body))]) Generated))

typeSigE :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
typeSigE expr ty = noLoc (ExprWithTySig NoExt expr (HsWC NoExt (HsIB NoExt ty)))

typeSig :: RdrName -> LHsType GhcPs -> LSig GhcPs
typeSig name ty = noLoc (TypeSig NoExt [noLoc name] (HsWC NoExt (HsIB NoExt ty)))

classOpSig :: RdrName -> LHsType GhcPs -> LSig GhcPs
classOpSig name ty = noLoc (ClassOpSig NoExt False [noLoc name] (HsIB NoExt ty))

tfInstanceD :: RdrName -> [LHsType GhcPs] -> LHsType GhcPs -> LTyFamInstDecl GhcPs
tfInstanceD name pats val = noLoc (TyFamInstDecl (HsIB NoExt (FamEqn NoExt (noLoc name) Nothing [HsValArg p | p <- pats] Prefix val)))

simpleFn :: RdrName -> LHsType GhcPs -> LHsExpr GhcPs -> [LHsDecl GhcPs]
simpleFn fnName ty body =
  [ noLoc (SigD NoExt (unLoc (typeSig fnName ty))),
    noLoc (ValD NoExt (unLoc (simpleBind fnName body)))
  ]

anyclassDeriving :: LHsType GhcPs -> LHsDerivingClause GhcPs
anyclassDeriving c = noLoc (HsDerivingClause NoExt (Just (noLoc AnyclassStrategy)) (noLoc [HsIB NoExt c]))

typeEq :: RdrName
typeEq = eqTyCon_RDR

simpleBind :: RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
simpleBind fnName body =
  let body' = simpleGHRSs body
      body'' = MG NoExt (noLoc [noLoc (Match NoExt (FunRhs (noLoc fnName) Prefix NoSrcStrict) [] body')]) Generated
   in noLoc (FunBind NoExt (noLoc fnName) body'' WpHole [])

instanceD_simple :: [LHsType GhcPs] -> LHsType GhcPs -> [LHsBind GhcPs] -> [LTyFamInstDecl GhcPs] -> LHsDecl GhcPs
instanceD_simple ctx head binds assocTypes = noLoc (InstD NoExt (ClsInstD NoExt inst))
  where
    inst =
      ClsInstDecl
        { cid_ext = NoExt,
          cid_poly_ty = HsIB NoExt (if null ctx then head else qualT ctx head),
          cid_binds = listToBag binds,
          cid_sigs = [],
          cid_tyfam_insts = assocTypes,
          cid_datafam_insts = [],
          cid_overlap_mode = Nothing
        }

classD_simple :: [LHsType GhcPs] -> RdrName -> [LHsTyVarBndr GhcPs] -> [LSig GhcPs] -> LHsDecl GhcPs
classD_simple ctx clsName clsVars sigs = noLoc (TyClD NoExt cls)
  where
    cls =
      ClassDecl
        { tcdCExt = NoExt,
          tcdCtxt = noLoc ctx,
          tcdLName = noLoc clsName,
          tcdTyVars = mkHsQTvs clsVars,
          tcdFixity = Prefix,
          tcdFDs = [],
          tcdSigs = sigs,
          tcdMeths = emptyBag,
          tcdATs = [],
          tcdATDefs = [],
          tcdDocs = []
        }

qimportD :: ModuleName -> LImportDecl GhcPs
qimportD moduleName = noLoc do
  ImportDecl
    { ideclExt = NoExt,
      ideclSourceSrc = NoSourceText,
      ideclName = noLoc moduleName,
      ideclPkgQual = Nothing,
      ideclSource = False,
      ideclSafe = False,
      ideclQualified = True,
      ideclImplicit = False,
      ideclAs = Nothing,
      ideclHiding = Nothing
    }

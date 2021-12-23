{-# LANGUAGE NamedFieldPuns #-}

module Data.Record.Plugin.Types where

import Data.Record.Plugin.GHC

data Record = Record
  { tyName :: RdrName,
    tyVars :: [HsTyVarBndr GhcPs],
    conName :: RdrName,
    fields :: [(RdrName, HsType GhcPs)],
    derivings :: [RecordDeriving]
  }

data StockDeriving = Eq | Show | Ord

data RecordDeriving
  = DeriveStock StockDeriving
  | DeriveAnyClass (LHsType GhcPs)

recordName :: Record -> String
recordName Record {tyName} = occNameString (rdrNameOcc tyName)

recordTy :: Record -> LHsType GhcPs
recordTy Record {tyName, tyVars} = appsT (varT tyName) [varT (hsTyVarName f) | f <- tyVars]

nameVectorFrom :: Record -> RdrName
nameVectorFrom rec = exprVarRdr ("vectorFrom" <> recordName rec)

nameVectorTo :: Record -> RdrName
nameVectorTo rec = exprVarRdr ("vectorTo" <> recordName rec)

nameUnsafeGetIndex :: Record -> RdrName
nameUnsafeGetIndex rec = exprVarRdr ("unsafeGetIndex" <> recordName rec)

nameUnsafeSetIndex :: Record -> RdrName
nameUnsafeSetIndex rec = exprVarRdr ("unsafeSetIndex" <> recordName rec)

nameConstraints :: Record -> RdrName
nameConstraints rec = mkRdrUnqual (mkTcOcc ("Constraints_" <> recordName rec))

nameDictConstraints :: Record -> RdrName
nameDictConstraints rec = exprVarRdr ("dictConstraints_" <> recordName rec)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Record.Plugin.Types where

import Data.Record.Plugin.GHC
import Data.Set (Set)
import qualified Data.Set as Set

data LargeRecordPluginException
  = DerivingWithoutStrategy
  | UnsupportedStockDeriving (HsType GhcPs)
  | UnsupportedStrategy (DerivStrategy GhcPs)
  | InvalidDeclaration
  | Untransformed (Set RdrName)

formatLargeRecordPluginException :: DynFlags -> LargeRecordPluginException -> String
formatLargeRecordPluginException dynFlags = \case
  DerivingWithoutStrategy ->
    "Only derivings with explicit strategy are supported"
  UnsupportedStockDeriving ty ->
    "Unsupported stock class: " ++ showSDoc dynFlags (ppr ty)
  UnsupportedStrategy strategy ->
    "Strategy " ++ showSDoc dynFlags (derivStrategyName strategy) ++ " is not supported"
  InvalidDeclaration ->
    "Unsupported declaration for large-records"
  Untransformed names ->
    unlines do
      "These large-record annotations were not applied: " : do
        name <- Set.toList names
        pure (" - " ++ rdrNameString name)

data Record = Record
  { tyName :: RdrName,
    tyVars :: [HsTyVarBndr GhcPs],
    conName :: RdrName,
    fields :: [(RdrName, HsType GhcPs)],
    derivings :: [RecordDeriving]
  }

data StockDeriving = Eq | Show | Ord | Generic

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

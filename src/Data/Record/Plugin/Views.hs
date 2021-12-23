{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Record.Plugin.Views (getLargeRecordsAnnos, viewRecord) where

import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.GHC
import Data.Record.Plugin.Types
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)

getLargeRecordsAnnos :: HsModule GhcPs -> Set RdrName
getLargeRecordsAnnos module_ = Set.fromList [tyName | LRAnn tyName <- Uniplate.universeBi module_]

pattern LRAnn :: RdrName -> AnnDecl GhcPs
pattern LRAnn tyName <- HsAnnotation _ _ (TypeAnnProvenance (L _ tyName)) (L _ (HsLit _ (HsString _ "large-record")))

viewRecordDerivings :: [LHsDerivingClause GhcPs] -> Maybe [RecordDeriving]
viewRecordDerivings = fmap concat . traverse viewRecordDeriving

viewRecordDeriving :: LHsDerivingClause GhcPs -> Maybe [RecordDeriving]
viewRecordDeriving (L _ c) = case c of
  HsDerivingClause {deriv_clause_strategy = Just (L _ AnyclassStrategy), deriv_clause_tys} ->
    Just [DeriveAnyClass c | HsIB _ c <- unLoc deriv_clause_tys]
  HsDerivingClause {deriv_clause_strategy = Just (L _ StockStrategy), deriv_clause_tys} ->
    for (hsib_body <$> unLoc deriv_clause_tys) \case
      L _ (HsTyVar _ _ (L _ ShowClass)) -> Just (DeriveStock Show)
      L _ (HsTyVar _ _ (L _ EqClass)) -> Just (DeriveStock Eq)
      L _ (HsTyVar _ _ (L _ OrdClass)) -> Just (DeriveStock Ord)
      _ -> Nothing
  _ -> Nothing

viewRecord :: LHsDecl GhcPs -> Maybe Record
viewRecord = \case
  L
    _
    ( TyClD
        _
        DataDecl
          { tcdLName = L _ tyName,
            tcdTyVars = HsQTvs {hsq_explicit = tyVars},
            tcdFixity = Prefix,
            tcdDataDefn =
              HsDataDefn
                { dd_ND = DataType,
                  dd_ctxt = L _ [],
                  dd_cType = Nothing,
                  dd_kindSig = Nothing,
                  dd_cons =
                    [ L
                        _
                        ConDeclH98
                          { con_name = L _ conName,
                            con_forall = L _ False,
                            con_ex_tvs = [],
                            con_mb_cxt = Nothing,
                            con_args = RecCon (L _ fields)
                          }
                      ],
                  dd_derivs = L _ (viewRecordDerivings -> Just derivings)
                }
          }
      ) ->
      Just
        Record
          { tyName,
            tyVars = unLoc <$> tyVars,
            conName,
            fields = [p | f <- fields, p <- viewFields f],
            derivings
          }
  _ -> Nothing

viewFields :: LConDeclField GhcPs -> [(RdrName, HsType GhcPs)]
viewFields (L _ ConDeclField {cd_fld_names, cd_fld_type = L _ ty}) =
  [(name, ty) | L _ FieldOcc {rdrNameFieldOcc = L _ name} <- cd_fld_names]
viewFields _ = []

pattern ShowClass, EqClass, OrdClass :: RdrName
pattern ShowClass <- ((\r -> rdrNameString r == "Show") -> True)
pattern EqClass <- ((\r -> rdrNameString r == "Eq") -> True)
pattern OrdClass <- ((\r -> rdrNameString r == "Ord") -> True)

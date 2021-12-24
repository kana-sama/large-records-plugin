{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Record.Plugin.Views
  ( viewDataDeclName,
    viewRecord,
    getLargeRecordsAnnos,
  )
where

import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.GHC
import Data.Record.Plugin.Types
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)

getLargeRecordsAnnos :: HsModule GhcPs -> Set RdrName
getLargeRecordsAnnos module_ = Set.fromList [tyName | LRAnn tyName <- Uniplate.universeBi module_]

viewDataDeclName :: LHsDecl GhcPs -> Maybe RdrName
viewDataDeclName = \case
  L _ (TyClD _ DataDecl {tcdLName = L _ tyName}) -> Just tyName
  _ -> Nothing

viewRecord :: LHsDecl GhcPs -> Either LargeRecordPluginException Record
viewRecord (L _ decl) = case decl of
  TyClD
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
              dd_derivs = L _ derivs
            }
      } -> do
      derivings <- viewRecordDerivings derivs
      pure
        Record
          { tyName,
            tyVars = unLoc <$> tyVars,
            conName,
            fields = [p | f <- fields, p <- viewFields f],
            derivings
          }
  _ -> Left InvalidDeclaration

viewFields :: LConDeclField GhcPs -> [(RdrName, HsType GhcPs)]
viewFields (L _ ConDeclField {cd_fld_names, cd_fld_type = L _ ty}) =
  [(name, ty) | L _ FieldOcc {rdrNameFieldOcc = L _ name} <- cd_fld_names]
viewFields _ = []

viewRecordDerivings :: [LHsDerivingClause GhcPs] -> Either LargeRecordPluginException [RecordDeriving]
viewRecordDerivings = fmap concat . traverse viewRecordDeriving

viewRecordDeriving :: LHsDerivingClause GhcPs -> Either LargeRecordPluginException [RecordDeriving]
viewRecordDeriving (L _ HsDerivingClause {deriv_clause_strategy, deriv_clause_tys}) =
  case deriv_clause_strategy of
    Nothing -> Left DerivingWithoutStrategy
    Just (L _ AnyclassStrategy) ->
      Right [DeriveAnyClass c | HsIB _ c <- unLoc deriv_clause_tys]
    Just (L _ StockStrategy) ->
      for (hsib_body <$> unLoc deriv_clause_tys) \case
        L _ (HsTyVar _ _ (L _ (Ident "Show"))) -> Right (DeriveStock Show)
        L _ (HsTyVar _ _ (L _ (Ident "Eq"))) -> Right (DeriveStock Eq)
        L _ (HsTyVar _ _ (L _ (Ident "Ord"))) -> Right (DeriveStock Ord)
        L _ (HsTyVar _ _ (L _ (Ident "Generic"))) -> Right (DeriveStock Generic)
        L _ ty -> Left (UnsupportedStockDeriving ty)
    Just (L _ strategy) -> Left (UnsupportedStrategy strategy)
viewRecordDeriving _ = Right []

pattern Ident :: String -> RdrName
pattern Ident x <- (rdrNameString -> x)

pattern LRAnn :: RdrName -> AnnDecl GhcPs
pattern LRAnn tyName <-
  HsAnnotation
    _
    _
    (TypeAnnProvenance (L _ tyName))
    (L _ (HsVar _ (L _ (rdrNameString -> "LargeRecord"))))

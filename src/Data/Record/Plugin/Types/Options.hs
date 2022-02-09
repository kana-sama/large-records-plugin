{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Record.Plugin.Types.Options
  ( LargeRecordOptions (..),
    shouldRecordBeStrict,
    shouldGeneratedHasField,
    getLargeRecordOptions,
  )
where

import Control.Applicative (empty)
import Data.Data (Data)
import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Record.Plugin.GHC

data LargeRecordOptions = LargeRecordStrict | LargeRecordLazy
  deriving stock (Data)

shouldRecordBeStrict :: LargeRecordOptions -> Bool
shouldRecordBeStrict LargeRecordStrict = True
shouldRecordBeStrict LargeRecordLazy = False

shouldGeneratedHasField :: LargeRecordOptions -> Bool
shouldGeneratedHasField _ = True

getLargeRecordOptions :: HsModule GhcPs -> Map RdrName LargeRecordOptions
getLargeRecordOptions module_ = Map.fromList do
  anno <- Uniplate.universeBi module_
  case viewLargeRecordAnnotation anno of
    Just (tyName, option) -> pure (tyName, option)
    Nothing -> empty

viewLargeRecordAnnotation :: AnnDecl GhcPs -> Maybe (RdrName, LargeRecordOptions)
viewLargeRecordAnnotation = \case
  TyAnno tyName (rdrNameString -> "LargeRecordStrict") ->
    Just (tyName, LargeRecordStrict)
  TyAnno tyName (rdrNameString -> "LargeRecordLazy") ->
    Just (tyName, LargeRecordLazy)
  _ -> Nothing

pattern TyAnno :: RdrName -> RdrName -> AnnDecl GhcPs
pattern TyAnno tyName value <-
  HsAnnotation _ _ (TypeAnnProvenance (L _ tyName)) (L _ (HsVar _ (L _ value)))

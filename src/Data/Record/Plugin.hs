{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Record.Plugin (plugin) where

import Control.Exception
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.CodeGen (genLargeRecord)
import Data.Record.Plugin.GHC
import Data.Record.Plugin.RuntimeNames (moduleGHCGeneric, moduleRuntime)
import Data.Record.Plugin.Types
import Data.Record.Plugin.Views
import Data.Set (Set)
import qualified Data.Set as Set

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction, pluginRecompile = purePlugin}
  where
    parsedResultAction options _ mod@HsParsedModule {hpm_module = L l module_} = do
      case transformDecls module_ of
        Right module_ -> do
          pure mod {hpm_module = L l (requiredImports module_)}
        Left err -> do
          dynFlags <- getDynFlags
          error (formatLargeRecordPluginException dynFlags err)

transformDecls :: HsModule GhcPs -> Either LargeRecordPluginException (HsModule GhcPs)
transformDecls mod@HsModule {hsmodDecls} = do
  let largeRecords = getLargeRecordsAnnos mod

  (hsmodDecls, transformed) <- foldFor hsmodDecls \decl ->
    case viewDataDeclName decl of
      Just tyName | tyName `Set.member` largeRecords -> do
        decl <- genLargeRecord <$> viewRecord decl
        pure (decl, Set.singleton tyName)
      _ -> pure ([decl], Set.empty)

  let untransformed = largeRecords `Set.difference` transformed
  unless (Set.null untransformed) do
    Left (Untransformed untransformed)

  pure mod {hsmodDecls}

requiredImports :: HsModule GhcPs -> HsModule GhcPs
requiredImports module_@HsModule {hsmodImports} =
  module_ {hsmodImports = hsmodImports ++ [qimportD m | m <- [moduleRuntime, moduleGHCGeneric]]}

foldFor :: (Applicative m, Traversable t, Monoid b) => t a -> (a -> m b) -> m b
foldFor xs f = fold <$> traverse f xs

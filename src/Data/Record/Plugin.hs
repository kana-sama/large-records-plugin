{-# LANGUAGE NamedFieldPuns #-}

module Data.Record.Plugin (plugin) where

import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.Record.Plugin.CodeGen (genLargeRecord)
import Data.Record.Plugin.GHC
import Data.Record.Plugin.RuntimeNames (moduleGHCGeneric, moduleRuntime)
import Data.Record.Plugin.Types (Record (..))
import Data.Record.Plugin.Views (getLargeRecordsAnnos, viewRecord)
import qualified Data.Set as Set

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction, pluginRecompile = purePlugin}
  where
    parsedResultAction options _ mod@HsParsedModule {hpm_module = L l module_} =
      pure mod {hpm_module = L l (addImports (transformDecls module_))}

transformDecls :: HsModule GhcPs -> HsModule GhcPs
transformDecls mod@HsModule {hsmodDecls} = mod {hsmodDecls = hsmodDecls >>= transform}
  where
    lrs = getLargeRecordsAnnos mod
    transform decl =
      case viewRecord decl of
        Just rec@Record {tyName} | tyName `Set.member` lrs -> genLargeRecord rec
        _ -> [decl]

addImports :: HsModule GhcPs -> HsModule GhcPs
addImports module_@HsModule {hsmodImports} =
  module_ {hsmodImports = hsmodImports ++ [qimportD m | m <- [moduleRuntime, moduleGHCGeneric]]}

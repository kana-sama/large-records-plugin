{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Record.Plugin (plugin) where

import qualified Data.Generics.Uniplate.Data as Uniplate
import Data.List (isSuffixOf, sort, stripPrefix)
import Data.Set (Set)
import qualified Data.Set as Set
import ErrUtils
import GhcPlugins
import HsSyn
import Lexer (P (unP), ParseResult (..), mkPState)
import Parser (parseIdentifier)
import StringBuffer (stringToStringBuffer)

parseError :: String
parseError = "invalid Data.Record.Plugin option"

parse :: P a -> String -> Hsc a
parse p source = do
  dynFlags <- getDynFlags
  fromParseResult (unP p (pstate dynFlags))
  where
    fromParseResult (POk _ r) = pure r
    fromParseResult (PFailed _ _ err) = do
      dynFlags <- getDynFlags
      liftIO do ErrUtils.putMsg dynFlags err
      error parseError

    filename = "<interactive>"
    source' = stringToStringBuffer source
    location = mkRealSrcLoc (mkFastString filename) 1 1
    pstate dynFlags = mkPState dynFlags source' location

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction, pluginRecompile = purePlugin}
  where
    parsedResultAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
    parsedResultAction clo _ mod@HsParsedModule {hpm_module = L l module_} = do
      options <- parseOptions clo
      let lrs = getLargeRecordsAnnos module_
      pure mod {hpm_module = L l (wrapDecls options lrs module_)}

type TyName = RdrName

getLargeRecordsAnnos :: HsModule GhcPs -> Set TyName
getLargeRecordsAnnos module_ = Set.fromList [tyName | LRAnn tyName <- Uniplate.universeBi module_]

wrapDecls :: Options -> Set TyName -> HsModule GhcPs -> HsModule GhcPs
wrapDecls options lrs = Uniplate.transformBi \case
  L l (TyClD _ decl@DataDecl {tcdLName = L _ tyName}) | tyName `Set.member` lrs -> wrapDecl options l decl
  decl -> decl

-- Misc

data Options = Options {via :: RdrName, arguments :: [RdrName]}

defaultOptions :: Options
defaultOptions = Options {via = rdrName "largeRecord", arguments = [rdrName "defaultPureScript"]}

data OptionElem
  = ViaOption RdrName
  | ArgsOption [RdrName]
  deriving (Eq, Ord)

parseOptionElem :: CommandLineOption -> Hsc OptionElem
parseOptionElem (stripPrefix "via=" -> Just rest) = ViaOption . unLoc <$> parse parseIdentifier rest
parseOptionElem (stripPrefix "args=[" -> Just (stripSuffix "]" -> Just rest)) =
  ArgsOption <$> traverse (fmap unLoc . parse parseIdentifier) (splitOn ',' rest)
parseOptionElem _ = error parseError

stripSuffix :: String -> String -> Maybe String
stripSuffix suf str | suf `isSuffixOf` str = Just (take (length str - length suf) str)
stripSuffix _ _ = Nothing

splitOn :: Char -> String -> [String]
splitOn d rest = case break (== d) rest of
  (value, []) -> [value]
  (value, rest) -> value : splitOn d (tail rest)

parseOptions :: [CommandLineOption] -> Hsc Options
parseOptions opts = do
  opts <- sort <$> traverse parseOptionElem opts
  (\f -> pure (foldl f defaultOptions opts)) \opts -> \case
    ViaOption via -> Options {via, arguments = []}
    ArgsOption arguments -> opts {arguments}

pattern LRAnn :: TyName -> AnnDecl GhcPs
pattern LRAnn tyName <- HsAnnotation _ _ (TypeAnnProvenance (L _ tyName)) (L _ (HsLit _ (HsString _ "large-record")))

wrapDecl :: Options -> SrcSpan -> TyClDecl GhcPs -> LHsDecl GhcPs
wrapDecl Options {via, arguments} loc ty =
  let splice =
        (HsUntypedSplice NoExt NoParens (mkRdrUnqual (mkVarOcc "splice")))
          let declQQ = noLoc (HsBracket NoExt (DecBrL NoExt [L loc (TyClD NoExt ty)])) :: LHsExpr GhcPs
           in app (var via) (fmap var arguments ++ [declQQ])
   in L loc (SpliceD NoExt (SpliceDecl NoExt (noLoc splice) ImplicitSplice))
  where
    var = noLoc . HsVar NoExt . noLoc
    app = foldl (\f arg -> noLoc (HsApp NoExt f arg))

rdrName :: String -> RdrName
rdrName = mkRdrUnqual . mkVarOcc

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
module Ide.Plugin.TH (descriptor) where
import           Data.Aeson                     (ToJSON)
import           Data.Aeson                     (Value (Null))
import           Data.Aeson                     (ToJSON (toJSON))
import           Data.Aeson.Types               (FromJSON)
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.Text                      as T
import           Development.IDE.Core.RuleTypes (TcModuleResult (tmrModule),
                                                 GhcSession(..),
                                                 TypeCheck (TypeCheck),
                                                 GetParsedModule(..))
import           Development.IDE.Core.Shake     (use_, IdeState (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (srcSpanToLocation)
import           Development.IDE.GHC.Util       (hscEnv)
import           GHC.Generics                   (Generic)
import           Ide.Plugin
import           Ide.Types
import           Language.Haskell.LSP.Types
import Development.IDE.Core.Service (runAction)
import Development.Shake (Action)
import Data.Maybe
import Bag
import HscTypes
import Outputable

thCommandId :: CommandId
thCommandId = "EvaluateTH"

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId) {
    pluginCodeLensProvider = Just provider,
    pluginCommands = [ thLensCommand ]
}

-- | The command descriptor
thLensCommand :: PluginCommand
thLensCommand =
    PluginCommand thCommandId "Evaluate TH" runTHCommand

data THCommandParams = THCommandParams Location
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

runTHCommand :: CommandFunction THCommandParams
runTHCommand _lspFuncs state (THCommandParams loc@Location{..}) = do
  let Just nfp = uriToNormalizedFilePath $ toNormalizedUri _uri
  (dflags, tmr) <- runIde state $ do
    dflags <- hsc_dflags . hscEnv <$> use_ GhcSession nfp
    tmr <- use_ TypeCheck nfp
    pure (dflags, tmr)
  let mrs = tm_renamed_source $ tmrModule tmr
      getEs :: Outputable a => Located a -> Maybe T.Text
      getEs (L span decl)
        | srcSpanToLocation span == Just loc = Just $ T.pack $ showSDoc dflags $ ppr decl
        | otherwise = Nothing
  let edits = case mrs of
        Nothing -> []
        Just (HsGroup{..},_,_,_)
          -> let (vals, sigs) = case hs_valds of
                  ValBinds _ binds sigs -> (bagToList binds, sigs)
                  XValBindsLR (NValBinds binds sigs) -> (concatMap (bagToList . snd) binds, sigs)
                 go TyClGroup{..}
                    = mapMaybe getEs group_tyclds
                   ++ mapMaybe getEs group_roles
                   ++ mapMaybe getEs group_instds
          in mapMaybe getEs vals
          ++ mapMaybe getEs sigs
          ++ concatMap go hs_tyclds
          ++ mapMaybe getEs hs_derivds
          ++ mapMaybe getEs hs_fixds
          ++ mapMaybe getEs hs_defds
          ++ mapMaybe getEs hs_fords
          ++ mapMaybe getEs hs_warnds
          ++ mapMaybe getEs hs_annds
          ++ mapMaybe getEs hs_ruleds
          ++ mapMaybe getEs hs_docs
  let editsMap = HashMap.fromList [(_uri, List [thEdit])]
      thEdit = TextEdit _range $ T.unlines edits
      edit = WorkspaceEdit (Just editsMap) Nothing
  pure (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edit))

provider :: CodeLensProvider
provider _lspFuncs state pId CodeLensParams{_textDocument = TextDocumentIdentifier{_uri}}
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri
  = do
    ps <- runIde state $ use_ GetParsedModule nfp
    let splices = extractSplices ps
    commands <- mapM (generateLens pId) splices
    pure $ Right $ List commands
  | otherwise
  = return $ Right (List [])

extractSplices :: ParsedModule -> [Location]
extractSplices pm = mapMaybe go decls
  where
    src = pm_parsed_source pm
    decls = hsmodDecls $ unLoc src
    go (L span SpliceD{}) = srcSpanToLocation span
    go _ = Nothing

generateLens :: PluginId -> Location -> IO CodeLens
generateLens pId loc@Location{..} = do
  let title = "Evaluate and splice in TH"
      _xdata = Nothing
      _arguments = Just [toJSON $ THCommandParams loc]
  _command <- Just <$> mkLspCommand pId thCommandId title _arguments
  return $ CodeLens{..}

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "thLens" state

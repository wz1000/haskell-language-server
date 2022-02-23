{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

-- | CoreFiles let us serialize Core to a file in order to later recover it
-- without reparsing or retypechecking
module Development.IDE.GHC.CoreFile  where

import Data.IORef
import Data.Foldable
import Data.List (isPrefixOf)
import Control.Monad.IO.Class
import Control.Monad

import Development.IDE.GHC.Compat

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Binary
import GHC.Core
import GHC.CoreToIface
import GHC.IfaceToCore
import GHC.Iface.Env
import GHC.Iface.Binary

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.TypeEnv
#else
import GHC.Driver.Types
#endif

#elif MIN_VERSION_ghc(8,6,0)
import Binary
import CoreSyn
import ToIface
import TcIface
import IfaceEnv
import BinIface
import HscTypes
import IdInfo
import Var
import Unique
#endif

import Outputable

-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024


data  CoreFile = CoreFile { cf_bindings :: [TopIfaceBinding IfaceId]
                          , cf_info :: [(IfaceTopBndr, GhcideIfaceDetails)]
                          }

type IfClass = IfaceTopBndr
type IfDataCon = IfaceTopBndr

data GhcideIfaceDetails
  = GhcideDataConWorkId IfDataCon       -- ^ The 'Id' is for a data constructor /worker/
  | GhcideDataConWrapId IfDataCon       -- ^ The 'Id' is for a data constructor /wrapper/

                                -- [the only reasons we need to know is so that
                                --  a) to support isImplicitId
                                --  b) when desugaring a RecordCon we can get
                                --     from the Id back to the data con]
  | GhcideClassOpId IfClass             -- ^ The 'Id' is a superclass selector,
                                -- or class operation of a class

  | GhcideCoVarId    -- ^ A coercion variable
               -- This only covers /un-lifted/ coercions, of type
               -- (t1 ~# t2) or (t1 ~R# t2), not their lifted variants
  | GhcideJoinId JoinArity           -- ^ An 'Id' for a join point taking n arguments

--  | GhcideFCallId ForeignCall         -- ^ The 'Id' is for a foreign call.
--                                -- Type will be simple: no type families, newtypes, etc

instance Binary GhcideIfaceDetails where
  put_ bh (GhcideDataConWorkId name) = do
    putByte bh 0
    put_ bh name
  put_ bh (GhcideDataConWrapId name) = do
    putByte bh 1
    put_ bh name
  put_ bh (GhcideClassOpId name) = do
    putByte bh 2
    put_ bh name
  put_ bh (GhcideCoVarId) = do
    putByte bh 3
  put_ bh (GhcideJoinId ar) = do
    putByte bh 4
    put_ bh ar

  get bh = do
    t <- getByte bh
    case t of
      0 -> GhcideDataConWorkId <$> get bh
      1 -> GhcideDataConWrapId <$> get bh
      2 -> GhcideClassOpId <$> get bh
      3 -> pure GhcideCoVarId
      4 -> GhcideJoinId <$> get bh
      _ -> error "binary: GhcideIfaceDetails"

type DetailsEnv = NameEnv IdDetails

mkDetailsEnv :: TypeEnv -> [(IfaceTopBndr, GhcideIfaceDetails)] -> DetailsEnv
mkDetailsEnv tenv xs = mkNameEnv [(n, tcIdInfo n v) | (n,v) <- xs]
  where
    tcIdInfo n (GhcideDataConWorkId dc) = case lookupTypeEnv tenv dc of
      Just (AConLike (RealDataCon con)) -> DataConWorkId con
    tcIdInfo n (GhcideDataConWrapId dc) = case lookupTypeEnv tenv dc of
      Just (AConLike (RealDataCon con)) -> DataConWrapId con
    tcIdInfo n (GhcideClassOpId cn) = case lookupTypeEnv tenv cn of
      Just (ATyCon (tyConClass_maybe -> Just cls)) -> ClassOpId cls
    tcIdInfo n (GhcideCoVarId) = CoVarId
    tcIdInfo n (GhcideJoinId ar) = JoinId ar

toGhcideIfaceDetails :: IdDetails -> Maybe GhcideIfaceDetails
toGhcideIfaceDetails (DataConWorkId dc) = Just $ GhcideDataConWorkId (dataConName dc)
toGhcideIfaceDetails (DataConWrapId dc) = Just $ GhcideDataConWrapId (dataConName dc)
toGhcideIfaceDetails (ClassOpId cls) = Just $ GhcideClassOpId (className cls)
toGhcideIfaceDetails CoVarId = Just $ GhcideCoVarId
toGhcideIfaceDetails (JoinId i) = Just $ GhcideJoinId i
toGhcideIfaceDetails _ = Nothing

data TopIfaceBinding v
  = TopIfaceNonRec v IfaceExpr
  | TopIfaceRec    [(v, IfaceExpr)]
  deriving (Functor, Foldable, Traversable)

-- | GHC doesn't export 'tcIdDetails', 'tcIfaceInfo', or 'tcIfaceType',
-- but it does export 'tcIfaceDecl'
-- so we use `IfaceDecl` as a container for all of these, as wel
-- invariant: 'IfaceId' is always a 'IfaceId' constructor
type IfaceId = IfaceDecl

instance Binary (TopIfaceBinding IfaceId) where
  put_ bh (TopIfaceNonRec d e) = do
    putByte bh 0
    put_ bh d
    put_ bh e
  put_ bh (TopIfaceRec vs) = do
    putByte bh 1
    put_ bh vs
  get bh = do
    t <- getByte bh
    case t of
      0 -> TopIfaceNonRec <$> get bh <*> get bh
      1 -> TopIfaceRec <$> get bh
      _ -> error "Binary TopIfaceBinding"

instance Binary CoreFile where
  put_ bh (CoreFile a b) = put_ bh a >> put_ bh b
  get bh = CoreFile <$> get bh <*> get bh

readBinCoreFile
  :: NameCacheUpdater
  -> FilePath
  -> IO CoreFile
readBinCoreFile name_cache fat_hi_path = do
    bh <- readBinMem fat_hi_path
    getWithUserData name_cache bh

-- | Write an interface file
writeBinCoreFile :: FilePath -> CoreFile -> IO ()
writeBinCoreFile core_path fat_iface = do
    bh <- openBinMem initBinMemSize

    let quietTrace =
#if MIN_VERSION_ghc(9,2,0)
          QuietBinIFace
#else
          (const $ pure ())
#endif

    putWithUserData quietTrace bh fat_iface

    -- And send the result to the file
    writeBinMem bh core_path

codeGutsToCoreFile :: CgGuts -> CoreFile
codeGutsToCoreFile CgGuts{..} = pprTrace "codeGutsToCoreFile" (ppr cg_binds) $
  uncurry CoreFile $ fmap concat $ unzip $ map (toIfaceTopBind cg_module) cg_binds

toIfaceTopBndr :: Module -> Id -> IfaceId
toIfaceTopBndr mod id
  = IfaceId (mangleDeclName mod $ getName id)
            (toIfaceType (idType id))
            (toIfaceIdDetails (idDetails id))
            (toIfaceIdInfo (idInfo id))

toIfaceTopBind :: Module -> Bind Id -> (TopIfaceBinding IfaceId, [(IfaceTopBndr,GhcideIfaceDetails)])
toIfaceTopBind mod (NonRec b r) =
  let ifb = toIfaceTopBndr mod b
    in (TopIfaceNonRec ifb (toIfaceExpr r), [(ifName ifb, dets)| Just dets <- pure $ toGhcideIfaceDetails $ idDetails b])
toIfaceTopBind mod (Rec prs)    =
  let ifns = [(toIfaceTopBndr mod b, toIfaceExpr r) | (b,r) <- prs]
  in (TopIfaceRec ifns, [(ifName ifn, dets) | ((id, _),(ifn,_)) <- zip prs ifns, Just dets <- pure $ toGhcideIfaceDetails $ idDetails id])

typecheckCoreFile :: Module -> IORef TypeEnv -> CoreFile -> IfG CoreProgram
typecheckCoreFile this_mod type_var (CoreFile prepd_binding dets) = do
  te <- liftIO $ readIORef type_var
  let de = mkDetailsEnv te dets
  initIfaceLcl this_mod (text "typecheckCoreFile") NotBoot $ do
    tcTopIfaceBindings type_var de prepd_binding

mangleDeclName :: Module -> Name -> Name
mangleDeclName mod name
  | isExternalName name = name
  | otherwise = mkExternalName (nameUnique name) (mangleModule (nameUnique name) mod) (nameOccName name) (nameSrcSpan name)

mangleModule :: Unique -> Module -> Module
mangleModule uniq mod = mkModule (moduleUnitId mod) (mkModuleName $ "GHCIDEINTERNAL" ++ moduleNameString (moduleName mod) ++ show (getKey uniq))

isGhcideModule :: Module -> Bool
isGhcideModule mod = "GHCIDEINTERNAL" `isPrefixOf` (moduleNameString $ moduleName mod)

isGhcideName :: Name -> Bool
isGhcideName = isGhcideModule . nameModule

tcTopIfaceBindings :: IORef TypeEnv -> DetailsEnv ->  [TopIfaceBinding IfaceId]
          -> IfL [CoreBind]
tcTopIfaceBindings ty_var de ver_decls
   = do
     int <- mapM (traverse $ tcIfaceId de) ver_decls
     let all_ids = concatMap toList int
     liftIO $ modifyIORef ty_var (flip extendTypeEnvList $ map AnId all_ids)
     xs <- extendIfaceIdEnv all_ids $ mapM tc_iface_bindings int
     pprTraceM "READ Iface" (ppr xs)
     pure xs

tcIfaceId :: DetailsEnv -> IfaceId -> IfL Id
tcIfaceId de ifn = fmap getIfaceId . tcIfaceDecl False =<< unmangle_decl_name ifn
  where
    unmangle_decl_name ifid@IfaceId{ ifName = name }
      | isGhcideName name = do
        name' <- newIfaceName (mkVarOcc $ getOccString name)
        pure $ ifid{ ifName = name' }
      | otherwise = pure ifid
    -- invariant: 'IfaceId' is always a 'IfaceId' constructor
    getIfaceId (AnId id) =
      let dets = case lookupNameEnv de (ifName ifn) of
            Just d -> d
            Nothing -> idDetails id
      in setIdDetails id dets
    getIfaceId _ = error "tcIfaceId: got non Id"

tc_iface_bindings :: TopIfaceBinding Id -> IfL CoreBind
tc_iface_bindings (TopIfaceNonRec v e) = do
  e' <- tcIfaceExpr e
  pure $ NonRec v e'
tc_iface_bindings (TopIfaceRec vs) = do
  vs' <- traverse (\(v, e) -> (,) <$> pure v <*> tcIfaceExpr e) vs
  pure $ Rec vs'

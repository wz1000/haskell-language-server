{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.TacticMachinery where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           DataCon
import           Development.IDE.Types.Location
import qualified FastString as FS
import           GHC
import           GHC.Generics
import           GHC.SourceGen.Overloaded
import           Ide.LocalBindings
import           Name
import           Outputable (ppr, showSDoc, Outputable)
import           Refinery.Tactic
import           TcType
import           Type
import           TysWiredIn (listTyCon, pairTyCon, intTyCon, floatTyCon, doubleTyCon, charTyCon)


------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType


------------------------------------------------------------------------------
-- | Given a 'SrcSpan' and a 'Bindings', create a hypothesis.
hypothesisFromBindings :: SrcSpan -> Bindings -> Map OccName CType
hypothesisFromBindings span (Bindings global local) =
  buildHypothesis global
    <> buildHypothesis (fromMaybe mempty $ M.lookup span local)


------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: Set Id -> Map OccName CType
buildHypothesis
  = M.fromList
  . fmap (occName &&& CType . varType)
  . filter (isAlpha . head . occNameString . occName)
  . S.toList


------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement = Judgement
  { jHypothesis :: Map OccName CType
  , jGoal       :: CType
  }
  deriving (Eq, Ord, Generic)


------------------------------------------------------------------------------
-- | Reasons a tactic might fail.
data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]
  | NoProgress

instance Show TacticError where
    show (UndefinedHypothesis name) = "undefined is not a function"
    show (GoalMismatch str typ) = "oh no"
    show (UnsolvedSubgoals jdgs) = "so sad"
    show NoProgress = "No Progress"


type ProvableM = ProvableT Judgement (Either TacticError)
type TacticsM = TacticT Judgement (LHsExpr GhcPs) ProvableM
type RuleM    = RuleT Judgement (LHsExpr GhcPs) ProvableM
type Rule     = RuleM (LHsExpr GhcPs)


------------------------------------------------------------------------------
-- | Produce a subgoal that must be solved before we can solve the original
-- goal.
newSubgoal
    :: Map OccName CType  -- ^ Available bindings
    -> CType              -- ^ Sub-goal type
    -> RuleM (LHsExpr GhcPs)
newSubgoal hy g = subgoal =<< newJudgement hy g


------------------------------------------------------------------------------
-- | Create a new judgment
newJudgement
    ::  Monad m
    => Map OccName CType  -- ^ Available bindings
    -> CType              -- ^ Sub-goal type
    -> m Judgement
newJudgement hy g = do
  pure $ Judgement hy g


------------------------------------------------------------------------------
-- | Produce a unique, good name for a type.
mkGoodName
    :: [OccName]  -- ^ Bindings in scope; used to ensure we don't shadow anything
    -> Type       -- ^ The type to produce a name for
    -> OccName
mkGoodName in_scope t =
  let tn = mkTyName t
   in mkVarOcc $ case elem (mkVarOcc tn) in_scope of
        True -> tn ++ show (length in_scope)
        False -> tn


------------------------------------------------------------------------------
-- | Use type information to create a reasonable name.
mkTyName :: Type -> String
-- eg. mkTyName (a -> B) = "fab"
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  = "f" ++ mkTyName a ++ mkTyName b
-- eg. mkTyName (a -> b -> C) = "f_C"
mkTyName (tcSplitFunTys -> ((_:_), b))
  = "f_" ++ mkTyName b
-- eg. mkTyName (Either A B) = "eab"
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = mkTyConName c ++ foldMap mkTyName args
-- eg. mkTyName a = "a"
mkTyName (getTyVar_maybe-> Just tv)
  = occNameString $ occName tv
-- eg. mkTyName (forall x. y) = "y"
mkTyName (tcSplitSigmaTy-> ((_:_), _, t))
  = mkTyName t
mkTyName _ = "x"


------------------------------------------------------------------------------
-- | Is this a function type?
isFunction :: Type -> Bool
isFunction (tcSplitFunTys -> ((_:_), _)) = True
isFunction _ = False

------------------------------------------------------------------------------
-- | Is this an algebraic type?
algebraicTyCon :: Type -> Maybe TyCon
algebraicTyCon (splitTyConApp_maybe -> Just (tycon, _))
  | tycon == intTyCon    = Nothing
  | tycon == floatTyCon  = Nothing
  | tycon == doubleTyCon = Nothing
  | tycon == charTyCon   = Nothing
  | tycon == funTyCon    = Nothing
  | otherwise = Just tycon
algebraicTyCon _ = Nothing


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String
mkTyConName tc
  | tc == listTyCon = "l_"
  | tc == pairTyCon = "p_"
  | otherwise = fmap toLower . take 1 . occNameString $ getOccName tc


------------------------------------------------------------------------------
-- | Attempt to generate a term of the right type using in-scope bindings, and
-- a given tactic.
runTactic
    :: DynFlags
    -> Judgement
    -> TacticsM ()       -- ^ Tactic to use
    -> Either TacticError (LHsExpr GhcPs)
runTactic dflags jdg t
  = fmap (parenthesize . fst)
  . runProvableT
  $ runTacticT t jdg


------------------------------------------------------------------------------
-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize a@(L _ HsVar{})        = a
parenthesize a@(L _ HsUnboundVar{}) = a
parenthesize a@(L _ HsOverLabel{})  = a
parenthesize a@(L _ HsOverLit{})    = a
parenthesize a@(L _ HsIPVar{})      = a
parenthesize a@(L _ HsLit{})        = a
parenthesize a = noLoc $ HsPar NoExt a


instance MonadExtract (LHsExpr GhcPs) ProvableM where
  hole = pure $ noLoc $ HsVar NoExt $ noLoc $ Unqual $ mkVarOcc "_"


------------------------------------------------------------------------------
-- | Which names are in scope?
getInScope :: Map OccName a -> [OccName]
getInScope = M.keys


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Map OccName CType  -- ^ In-scope bindings
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (LHsExpr GhcPs)
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal hy . CType) args
  pure
    . noLoc
    . foldl' (@@)
        (HsVar NoExt $ noLoc $ Unqual $ nameOccName $ dataConName dc)
    $ fmap unLoc sgs

render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr

-- TODO(sandy): this doesn't belong here
-- | Convert a DAML compiler Range to a GHC SrcSpan
rangeToSrcSpan :: String -> Range -> SrcSpan
rangeToSrcSpan file (Range (Position startLn startCh) (Position endLn endCh)) =
    mkSrcSpan
      (mkSrcLoc (FS.fsLit file) (startLn + 1) (startCh + 1))
      (mkSrcLoc (FS.fsLit file) (endLn + 1) (endCh + 1))


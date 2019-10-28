module Rextra.Dfa (
  -- * Deterministic Finite Automaton
    Dfa
  , State(..)
  , StateMap
  -- ** Constructing
  , dfa
  , dfa'
  -- ** Properties
  , stateMap
  , entryState
  , exitStates
  , transitionsByState
  -- ** Executing
  , transition
  , execute
  -- ** Renaming
  , rename
  ) where

import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Tuple

import           Rextra.Util

{-
 - Types
 -}

data Dfa s t = Dfa
  { stateMap   :: StateMap s t
  , entryState :: s
  } deriving (Show)

getState :: (Ord s) => Dfa s t -> s -> State s t
getState dfa s = stateMap dfa Map.! s

exitStates :: (Ord s) => Dfa s t -> Set.Set s
exitStates dfa = Set.fromList
               . map fst
               . filter (accepting . snd)
               . Map.assocs
               $ stateMap dfa

data State s t = State
  { transitions       :: Map.Map t s
  , defaultTransition :: s
  , accepting         :: Bool
  } deriving (Show)

type StateMap s t = Map.Map s (State s t)

fromMonoidalList :: (Monoid m, Ord k) => [(k, m)] -> Map.Map k m
fromMonoidalList = foldl' insertMonoidal Map.empty
  where
    insertMonoidal :: (Monoid m, Ord k) => Map.Map k m -> (k, m) -> Map.Map k m
    insertMonoidal map (k, m) = Map.insertWith mappend k m map

groupByFirst :: (Ord a, Ord b) => [(a, b)] -> Map.Map a (Set.Set b)
groupByFirst pairs =
  let prepared = map (\(a, b) -> (a, Set.singleton b)) pairs
  in  fromMonoidalList prepared

transitionsByState :: (Ord s, Ord t) => Map.Map t s -> Map.Map s (Set.Set t)
transitionsByState = groupByFirst . map swap . Map.assocs

{-
 - Constructing
 -}

integrityCheck :: (Ord s) => Dfa s t -> Bool
integrityCheck dfa =
  let states = Map.elems $ stateMap dfa
      transitionStates = concatMap (Map.elems . transitions) states
      defaultTransitionStates = map defaultTransition states
      referencedStates = Set.fromList $ concat [[entryState dfa], transitionStates, defaultTransitionStates]
  in  referencedStates `Set.isSubsetOf` Map.keysSet (stateMap dfa)

dfa :: (Ord s) => StateMap s t -> s -> Maybe (Dfa s t)
dfa stateMap entryState =
  let myDfa = Dfa{stateMap=stateMap, entryState=entryState}
  in  if integrityCheck myDfa then Just myDfa else Nothing

dfa' :: (Ord s, Ord t) => [(s, [(t, s)], s, Bool)] -> s -> Maybe (Dfa s t)
dfa' states entryState =
  let stateList = map (\(s, ts, dt, a) -> (s, State (Map.fromList ts) dt a)) states
  in  dfa (Map.fromList stateList) entryState

{-
 - Executing
 -}

transition :: (Ord s, Ord t) => Dfa s t -> s -> t -> s
transition dfa s t =
  let state = getState dfa s
  in  case transitions state Map.!? t of
        (Just nextState) -> nextState
        Nothing          -> defaultTransition state

execute :: (Ord s, Ord t) => Dfa s t -> [t] -> Bool
execute dfa tokens =
  let finalState = foldl' (transition dfa) (entryState dfa) tokens
  in  accepting $ getState dfa finalState

{-
 - Renaming
 -}

renameState :: (Ord s, Ord t) => State s t -> Rename s (State Int t)
renameState state = do
  newTransitions <- renameValues getName $ transitions state
  newDefaultTransition <- getName $ defaultTransition state
  pure $ State { transitions       = newTransitions
               , defaultTransition = newDefaultTransition
               , accepting         = accepting state
               }

renameAssoc :: (Ord s, Ord t) => (s, State s t) -> Rename s (Int, State Int t)
renameAssoc (name, state) = (,) <$> getName name <*> renameState state

rename :: (Ord s, Ord t) => Dfa s t -> Dfa Int t
rename dfa = doRename $ do
  newStateMap <- renameMap renameAssoc $ stateMap dfa
  newEntryState <- getName $ entryState dfa
  pure $ Dfa { stateMap = newStateMap, entryState = newEntryState }

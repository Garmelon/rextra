module Rextra.Dfa
  ( Dfa
  , dfa
  , dfa'
  , stateMap
  , entryState
  , transition
  , State(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data State s t = State
  { transitions       :: Map.Map t s
  , defaultTransition :: s
  , accepting         :: Bool
  }

data Dfa s t = Dfa
  { stateMap   :: Map.Map s (State s t)
  , entryState :: s
  }

{-
 - Constructing a DFA
 -}

integrityCheck :: (Ord s) => Dfa s t -> Bool
integrityCheck dfa =
  let states = Map.elems $ stateMap dfa
      transitionStates = concatMap (Map.elems . transitions) states
      defaultTransitionStates = map defaultTransition states
      referencedStates = Set.fromList $ concat [[entryState dfa], transitionStates, defaultTransitionStates]
  in  referencedStates `Set.isSubsetOf` Map.keysSet (stateMap dfa)

dfa :: (Ord s) => Map.Map s (State s t) -> s -> Maybe (Dfa s t)
dfa stateMap entryState =
  let myDfa = Dfa{stateMap=stateMap, entryState=entryState}
  in  if integrityCheck myDfa then Just myDfa else Nothing

dfa' :: (Ord s) => [(s, State s t)] -> s -> Maybe (Dfa s t)
dfa' states entryState = dfa (Map.fromList states) entryState

{-
 - "Executing" a DFA
 -}

getState :: (Ord s) => Dfa s t -> s -> State s t
getState dfa s = stateMap dfa Map.! s

transition :: (Ord s, Ord t) => Dfa s t -> s -> t -> s
transition dfa s t =
  let state = getState dfa s
  in  case transitions state Map.!? t of
        (Just nextState) -> nextState
        Nothing          -> defaultTransition state

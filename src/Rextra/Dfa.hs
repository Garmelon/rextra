{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rextra.Dfa
  ( Dfa
  , dfa
  , State(..)
  , stateTransition
  , normalize
  , mapState
  , transitionsByState
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Tuple

import           Rextra.Fa
import           Rextra.Util

-- State stuff

data State s t = State
  { transitions       :: Map.Map t s
  , defaultTransition :: s
  } deriving (Show, Eq, Ord)

stateTransition :: (Ord t) => State s t -> t -> s
stateTransition state token =
  case transitions state Map.!? token of
    Nothing -> defaultTransition state
    Just s  -> s

normalize :: (Eq s) => State s t -> State s t
normalize State{transitions, defaultTransition} =
  State { transitions = Map.filter (/= defaultTransition) transitions
        , defaultTransition
        }

mapState :: (s1 -> s2) -> State s1 t -> State s2 t
mapState f State{transitions, defaultTransition} =
  State { transitions       = Map.map f transitions
        , defaultTransition = f defaultTransition
        }

instance FaState State where
  canReach State{transitions, defaultTransition} =
    Set.fromList $ defaultTransition : Map.elems transitions

transitionsByState :: (Ord s, Ord t) => State s t -> Map.Map s (Set.Set t)
transitionsByState = groupByFirst . map swap . Map.assocs . transitions

-- Dfa stuff

type Dfa s t = Fa State s t

instance (Ord s) => Executable (Fa State s) s where
  startState = entryState
  transition a s = stateTransition (getState a s)
  accepts a s = s `Set.member` exitStates a

dfa :: (Ord s, Ord t) => [(s, [(t, s)], s)] -> s -> [s] -> Maybe (Dfa s t)
dfa stateInfo entryState exitStates =
  let stateList = map (\(s, ts, dt) -> (s, State (Map.fromList ts) dt)) stateInfo
  in  fa (Map.fromList stateList) entryState (Set.fromList exitStates)

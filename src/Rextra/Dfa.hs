{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rextra.Dfa
  ( Dfa
  , dfa
  , State(..)
  , transitionsByState
  ) where

import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Tuple

import           Rextra.Fa
import           Rextra.Util

-- State stuff

data State s t = State
  { transitions       :: Map.Map t s
  , defaultTransition :: s
  } deriving (Show)

instance FaState State where
  canReach State{transitions, defaultTransition} =
    Set.fromList $ defaultTransition : Map.elems transitions

transitionsByState :: (Ord s, Ord t) => State s t -> Map.Map s (Set.Set t)
transitionsByState = groupByFirst . map swap . Map.assocs . transitions

-- Dfa stuff

type Dfa s t = Fa State s t

instance (Ord s) => Executable (Fa State s) s where
  startState = entryState
  transition = dfaTransition
  accepts a s = s `Set.member` exitStates a

dfaTransition :: (Ord s, Ord t) => Dfa s t -> s -> t -> s
dfaTransition a s t =
  let state = getState a s
  in  case transitions state Map.!? t of
        (Just nextState) -> nextState
        Nothing          -> defaultTransition state

dfa :: (Ord s, Ord t) => [(s, [(t, s)], s)] -> s -> [s] -> Maybe (Dfa s t)
dfa states entryState exitStates =
  let stateList = map (\(s, ts, dt) -> (s, State (Map.fromList ts) dt)) states
  in  fa (Map.fromList stateList) entryState (Set.fromList exitStates)

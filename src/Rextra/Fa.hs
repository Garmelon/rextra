{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains ways to represent finite automata and their
-- execution/evaluation.

module Rextra.Fa
  ( FaState(..)
  , Fa
  , fa
  , stateMap
  , entryState
  , exitStates
  , states
  , getState
  , Executable(..)
  , transitionAll
  , execute
  ) where

import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | The state of a finite automaton.
class FaState state where
  -- | All the states that can be reached (by any sort of transition)
  -- from this state.
  canReach :: state s t -> Set.Set s

-- | A finite automaton.
data Fa state s t = Fa
  { stateMap   :: Map.Map s (state s t)
  -- ^ A 'Map.Map' from state indentifiers to states. See 'states',
  -- 'getState'.
  , entryState :: s
  -- ^ The state at which execution of the automaton begins.
  , exitStates :: Set.Set s
  -- ^ The automaton's accepting states, i. e. the states that
  -- determine whether the automaton accepts a certain word.
  }

-- | @'states' fa@ are the identifiers of all states contained in @fa@.
states :: Fa state s t -> Set.Set s
states Fa{stateMap} = Map.keysSet stateMap

-- | @'getState' fa s@ retrieves the @state s t@ corresponding to the
-- identifer @s@.
--
-- __Warning__: This function is unsafe. It only works if @s@ is a
-- member of @'states' fa@, and errors otherwise.
getState :: (Ord s) => Fa state s t -> s -> state s t
getState Fa{stateMap} s = stateMap Map.! s

integrityCheck :: (FaState state, Ord s) => Fa state s t -> Bool
integrityCheck fa'@Fa{stateMap, entryState, exitStates} =
  let reachable = map canReach $ Map.elems stateMap
      mentioned = Set.unions $ Set.singleton entryState : exitStates : reachable
  in  mentioned `Set.isSubsetOf` states fa'

-- | Construct a 'Fa'.
--
-- This constructor function also checks whether the finite
-- automaton's invariants (see 'getState') hold true.
fa :: (FaState state, Ord s) => Map.Map s (state s t) -> s -> Set.Set s -> Maybe (Fa state s t)
fa stateMap entryState exitStates =
  let potentialFa = Fa{stateMap, entryState, exitStates}
  in  if integrityCheck potentialFa then Just potentialFa else Nothing

-- | A special type class for automata that can be executed. These
-- automata must not necessarily be finite.
class Executable a execState where
  -- | The state at which execution of the automaton begins.
  startState :: a s t -> execState
  -- | A function that determines the automaton's next state based on
  -- a token.
  transition :: a s t -> execState -> t -> execState
  -- | Whether the automaton acceps
  accepts    :: a s t -> execState -> Bool

-- | Perform all transitions corresponding to a word (or list) of tokens, in order.
transitionAll :: (Executable a execState) => a s t -> execState -> [t] -> execState
transitionAll a = foldl' (transition a)

-- | Like 'transitionAll', starting with the automaton's 'startState'.
--
-- To check, whether an automaton @a@ accepts a word @w@, use
-- 'accepts' like this:
--
-- > a `accepts` execute a w
execute :: (Executable a execState) => a s t -> [t] -> execState
execute a = transitionAll a (startState a)

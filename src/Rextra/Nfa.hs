module Rextra.Nfa (
  -- * Nondeterministic Finite Automaton
    Nfa
  , State
  -- ** Constructing
  , nfa
  , nfa'
  -- ** Using
  , stateMap
  , entryState
  , exitStates
  , transition
  , execute
  -- ** Transitions
  , TransitionCondition(..)
  , specialStates
  , accepts
  ) where

import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | This condition determines which tokens a state transition applies to.
--
-- This representation is based on the assumption that there can be an
-- infinite number of different tokens. The condition thus contains a
-- default answer to the question "Does this transition apply to this
-- token?", and all the exceptions for which the answer is negated.
data TransitionCondition t
  = Only (Set.Set t)
  | AllExcept (Set.Set t)
  deriving (Show)

-- | The states which are treated differently from the default by the
-- 'TransitionCondition'.
specialStates :: TransitionCondition t -> Set.Set t
specialStates (Only s)      = s
specialStates (AllExcept s) = s

-- | Whether the condition holds true for a token.
accepts :: (Ord t) => TransitionCondition t -> t -> Bool
accepts (Only s)      t = Set.member t s
accepts (AllExcept s) t = Set.notMember t s

-- | A state consists of the transitions to other states, and the
-- conditions under which those transitions happen.
type State s t = [(TransitionCondition t, s)]

-- | A type representing a nondeterministic finite automaton.
--
-- It has one entry state and any number of exit states, which can be
-- interpreted as accepting states when the NFA is run.
data Nfa s t = Nfa
  { stateMap   :: Map.Map s (State s t)
  , entryState :: s
  , exitStates :: Set.Set s
  } deriving (Show)

{-
 - Constructing a NFA
 -}

integrityCheck :: (Ord s) => Nfa s t -> Bool
integrityCheck nfa =
  let referencedStates = Set.unions
        [ Set.singleton (entryState nfa)
        , exitStates nfa
        , Set.fromList . map snd . concat . Map.elems $ stateMap nfa
        ]
  in  referencedStates `Set.isSubsetOf` Map.keysSet (stateMap nfa)

-- | Construct an 'Nfa' from all its components.
--
-- This constructor function performs some error checking required to
-- keep the data structure internally consistent. At the moment, this
-- is limited to checking whether all state names mentioned anywhere
-- in the data struture actually exist in the state map.
nfa :: (Ord s)
    => Map.Map s (State s t) -- ^ The state lookup map (maps state name to state itself)
    -> s                     -- ^ The entry state (starting state)
    -> Set.Set s             -- ^ The exit states
    -> Maybe (Nfa s t)       -- ^ The 'Nfa', if the data didn't show any inconsistencies
nfa stateMap entryState exitStates =
  let myNfa = Nfa{stateMap=stateMap, entryState=entryState, exitStates=exitStates}
  in  if integrityCheck myNfa then Just myNfa else Nothing

-- | A version of 'nfa' using argument formats that should be easier to work with.
nfa' :: (Ord s) => [(s, State s t)] -> s -> [s] -> Maybe (Nfa s t)
nfa' states entryState exitStates = nfa (Map.fromList states) entryState (Set.fromList exitStates)

{-
 - "Executing" a NFA
 -}

getState :: (Ord s) => Nfa s t -> s -> State s t
getState nfa s = stateMap nfa Map.! s

-- | Starting from a state, find all the states that it can transition to with token @t@.
nextStates :: (Ord s, Ord t) => State s t -> t -> Set.Set s
nextStates state t = Set.fromList . map snd . filter (\(cond, _) -> cond `accepts` t) $ state

-- | The NFA's transition function.
--
-- Since this is a /nondeterministic/ finite automaton, the transition
-- function does not operate on individual states, but rather on a set
-- of current states.
--
-- __Warning__: This function does /not/ check whether the states
-- actually exist in the automaton, and it crashes if an invalid state
-- is used.
transition :: (Ord s, Ord t) => Nfa s t -> Set.Set s -> t -> Set.Set s
transition nfa ss t = foldMap (\s -> nextStates (getState nfa s) t) ss

execute :: (Ord s, Ord t) => Nfa s t -> [t] -> Bool
execute nfa tokens =
  let entryStates = Set.singleton $ entryState nfa
      finalStates = foldl' (transition nfa) entryStates tokens
  in  not $ Set.disjoint finalStates (exitStates nfa)

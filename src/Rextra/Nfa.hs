module Rextra.Nfa (
  -- * Nondeterministic Finite Automaton
    Nfa
  , State
  -- ** Constructing
  , nfa
  , nfa'
  -- ** Properties
  , stateMap
  , entryState
  , exitStates
  -- ** Executing
  , NdState
  , entryNdState
  , getNdState
  , accepting
  , transition
  , defaultTransition
  , execute
  -- *** Transition conditions
  , TransitionCondition(..)
  , specialTokens
  , accepts
  ) where

import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-
 - Types
 -}

-- | A type representing a nondeterministic finite automaton.
--
-- It has one entry state and any number of exit states, which can be
-- interpreted as accepting states when the NFA is run.
data Nfa s t = Nfa
  { stateMap   :: Map.Map s (State s t)
  , entryState :: s
  , exitStates :: Set.Set s
  } deriving (Show)

getState :: (Ord s) => Nfa s t -> s -> State s t
getState nfa s = stateMap nfa Map.! s

-- | A state consists of the transitions to other states, and the
-- conditions under which those transitions happen.
type State s t = [(TransitionCondition t, s)]

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

-- | The tokens which are treated differently from the default by the
-- 'TransitionCondition'.
specialTokens :: TransitionCondition t -> Set.Set t
specialTokens (Only tSet)      = tSet
specialTokens (AllExcept tSet) = tSet

-- | Whether the condition holds true for a token.
accepts :: (Ord t) => TransitionCondition t -> t -> Bool
accepts (Only s)      t = Set.member t s
accepts (AllExcept s) t = Set.notMember t s

{-
 - Constructing an NFA
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

-- | The nondeterministic (nd) current state of an NFA.
--
-- This type is used when executing a NFA.
type NdState s = Set.Set s

entryNdState :: Nfa s t -> NdState s
entryNdState = Set.singleton . entryState

getNdState :: (Ord s) => Nfa s t -> NdState s -> [State s t]
getNdState nfa ns = map (getState nfa) $ Set.toList ns

accepting :: (Ord s) => Nfa s t -> NdState s -> Bool
accepting nfa ns = not $ Set.disjoint ns (exitStates nfa)

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
transition :: (Ord s, Ord t) => Nfa s t -> NdState s -> t -> NdState s
transition nfa ns t = foldMap (\s -> nextStates s t) $ getNdState nfa ns

defaultTransition :: (Ord s) => Nfa s t -> NdState s -> NdState s
defaultTransition nfa ns = Set.fromList
                         . map snd
                         . filter (isAllExcept . fst)
                         . concat
                         $ getNdState nfa ns
  where
    isAllExcept :: TransitionCondition t -> Bool
    isAllExcept (AllExcept _) = True
    isAllExcept _             = False

execute :: (Ord s, Ord t) => Nfa s t -> [t] -> Bool
execute nfa tokens =
  let finalNdState = foldl' (transition nfa) (entryNdState nfa) tokens
  in  accepting nfa finalNdState

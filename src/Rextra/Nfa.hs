{-# LANGUAGE TupleSections #-}

module Rextra.Nfa (
  -- * Nondeterministic Finite Automaton
    Nfa
  , State(..)
  , StateMap
  , TransitionCondition(..)
  , specialTokens
  , accepts
  -- ** Constructing
  , only
  , allExcept
  , nfa
  , nfa'
  -- ** Properties
  , stateMap
  , entryState
  , exitStates
  -- ** Executing
  , NdState
  , getNdState
  -- *** Transitions
  , transition
  , defaultTransition
  -- *** Running the whole automaton
  , entryNdState
  , accepting
  , execute
  -- ** Renaming
  , rename
  ) where

import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Rextra.Util

{-
 - Types
 -}

-- | A type representing a nondeterministic finite automaton.
--
-- It has one entry state and any number of exit states, which can be
-- interpreted as accepting states when the NFA is run.
data Nfa s t = Nfa
  { stateMap   :: StateMap s t
  , entryState :: s
  , exitStates :: Set.Set s
  } deriving (Show)

getState :: (Ord s) => Nfa s t -> s -> State s t
getState nfa s = stateMap nfa Map.! s

data State s t = State
  { transitions        :: [(TransitionCondition t, s)]
  , epsilonTransitions :: Set.Set s
  } deriving (Show)

type StateMap s t = Map.Map s (State s t)

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

specialTokens :: TransitionCondition t -> Set.Set t
specialTokens (Only tSet)      = tSet
specialTokens (AllExcept tSet) = tSet

-- | Whether the condition holds true for a token.
accepts :: (Ord t) => TransitionCondition t -> t -> Bool
accepts (Only s)      t = Set.member t s
accepts (AllExcept s) t = Set.notMember t s

{-
 - Constructing
 -}

integrityCheck :: (Ord s) => Nfa s t -> Bool
integrityCheck nfa =
  let states = Map.elems $ stateMap nfa
      referencedStates = Set.unions $
        [ Set.singleton (entryState nfa)
        , exitStates nfa
        , Set.fromList . map snd $ concatMap transitions states
        ] <> map epsilonTransitions states
  in  referencedStates `Set.isSubsetOf` Map.keysSet (stateMap nfa)

-- | Construct an 'Nfa' from all its components.
--
-- This constructor function performs some error checking required to
-- keep the data structure internally consistent. At the moment, this
-- is limited to checking whether all state names mentioned anywhere
-- in the data struture actually exist in the state map.
nfa :: (Ord s)
    => StateMap s t    -- ^ The state lookup map (maps state name to state itself)
    -> s               -- ^ The entry state (starting state)
    -> Set.Set s       -- ^ The exit states
    -> Maybe (Nfa s t) -- ^ The 'Nfa', if the data didn't show any inconsistencies
nfa stateMap entryState exitStates =
  let myNfa = Nfa{stateMap=stateMap, entryState=entryState, exitStates=exitStates}
  in  if integrityCheck myNfa then Just myNfa else Nothing

only :: (Ord t) => [t] -> TransitionCondition t
only = Only . Set.fromList

allExcept :: (Ord t) => [t] -> TransitionCondition t
allExcept = AllExcept . Set.fromList

nfa' :: (Ord s)
     => [(s, [(TransitionCondition t, s)], [s])]
     -> s
     -> [s]
     -> Maybe (Nfa s t)
nfa' states entryState exitStates =
  let stateList = map (\(s, ts, et) -> (s, State ts (Set.fromList et))) states
  in  nfa (Map.fromList stateList) entryState (Set.fromList exitStates)

{-
 - Executing
 -}

-- | The nondeterministic (nd) current state of an NFA.
--
-- This type is used when executing a NFA.
type NdState s = Set.Set s

getNdState :: (Ord s) => Nfa s t -> NdState s -> [State s t]
getNdState nfa ns = map (getState nfa) $ Set.toList ns

-- Transitions

epsilonStep :: (Ord s) => Nfa s t -> NdState s -> NdState s
epsilonStep nfa ns = connectedElements (epsilonTransitions . getState nfa) ns

tokenStep :: (Ord s, Ord t) => Nfa s t -> t -> NdState s -> NdState s
tokenStep nfa t ns = foldMap (nextStates t) $ getNdState nfa ns
  where
    nextStates :: (Ord s, Ord t) => t -> State s t -> Set.Set s
    nextStates t state = Set.fromList
                       . map snd
                       . filter (\(cond, _) -> cond `accepts` t)
                       $ transitions state

defaultStep :: (Ord s) => Nfa s t -> NdState s -> NdState s
defaultStep nfa ns = Set.fromList
                   . map snd
                   . filter (isAllExcept . fst)
                   . concatMap transitions
                   $ getNdState nfa ns
  where
    isAllExcept :: TransitionCondition t -> Bool
    isAllExcept (AllExcept _) = True
    isAllExcept _             = False

-- | The NFA's transition function.
--
-- Since this is a /nondeterministic/ finite automaton, the transition
-- function does not operate on individual states, but rather on a set
-- of current states.
--
-- __Warning__: This function does /not/ check whether the states
-- actually exist in the automaton, and it crashes if an invalid state
-- is used.
transition :: (Ord s, Ord t) => Nfa s t -> t -> NdState s -> NdState s
transition nfa t = epsilonStep nfa . tokenStep nfa t . epsilonStep nfa

defaultTransition :: (Ord s) => Nfa s t -> NdState s -> NdState s
defaultTransition nfa = epsilonStep nfa . defaultStep nfa . epsilonStep nfa

-- Actually executing

entryNdState :: Nfa s t -> NdState s
entryNdState = Set.singleton . entryState

accepting :: (Ord s) => Nfa s t -> NdState s -> Bool
accepting nfa ns = not $ Set.disjoint ns (exitStates nfa)

execute :: (Ord s, Ord t) => Nfa s t -> [t] -> Bool
execute nfa tokens =
  let finalNdState = foldl' (flip $ transition nfa) (entryNdState nfa) tokens
  in  accepting nfa finalNdState

{-
 - Renaming
 -}

renameTransition :: (Ord s) => (TransitionCondition t, s) -> Rename s (TransitionCondition t, Int)
renameTransition (cond, s) = (cond,) <$> getName s

renameState :: (Ord s) => State s t -> Rename s (State Int t)
renameState state = do
  newTransitions <- mapM renameTransition $ transitions state
  newEpsilonTransitions <- renameSet getName $ epsilonTransitions state
  pure $ State { transitions = newTransitions, epsilonTransitions = newEpsilonTransitions }

renameAssoc :: (Ord s, Ord t) => (s, State s t) -> Rename s (Int, State Int t)
renameAssoc (name, state) = (,) <$> getName name <*> renameState state

rename :: (Ord s, Ord t) => Nfa s t -> Nfa Int t
rename nfa = doRename $ do
  newStateMap <- renameMap renameAssoc $ stateMap nfa
  newEntryState <- getName $ entryState nfa
  newExitStates <- renameSet getName $ exitStates nfa
  pure $ Nfa { stateMap   = newStateMap
             , entryState = newEntryState
             , exitStates = newExitStates
             }

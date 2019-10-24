module Rextra.Nfa where
-- TODO don't export internals

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data TransitionCondition t
  = Only (Set.Set t)
  | AllExcept (Set.Set t)

specialStates :: TransitionCondition t -> Set.Set t
specialStates (Only s)      = s
specialStates (AllExcept s) = s

accepts :: (Ord t) => TransitionCondition t -> t -> Bool
accepts (Only s)      t = Set.member t s
accepts (AllExcept s) t = Set.notMember t s

type State s t = [(TransitionCondition t, s)]

data Nfa s t = Nfa
  { nfaStates     :: Map.Map s (State s t)
  , nfaEntryState :: s
  , nfaExitStates :: Set.Set s
  }

{-
 - Constructing and modifying a NFA
 -}

-- TODO

{-
 - "Executing" a NFA
 -}

getState :: (Ord s) => Nfa s t -> s -> State s t
getState nfa s = nfaStates nfa Map.! s

-- * Starting from a state, find all the states that it can transition to with token 't'.
nextStates :: (Ord s, Ord t) => State s t -> t -> Set.Set s
nextStates state t = Set.fromList . map snd . filter (\(cond, _) -> cond `accepts` t) $ state

nfaTransition :: (Ord s, Ord t) => Nfa s t -> Set.Set s -> t -> Set.Set s
nfaTransition nfa ss t = foldMap (\s -> nextStates (getState nfa s) t) ss

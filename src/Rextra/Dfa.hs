module Rextra.Dfa where
-- TODO don't export internals

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data State s t = State
  { transitions       :: Map.Map t s
  , defaultTransition :: s
  , accepting         :: Bool
  }

data Dfa s t = Dfa
  { dfaStates     :: Map.Map s (State s t)
  , dfaEntryState :: s
  }

{-
 - Constructing and modifying a DFA
 -}

-- TODO

{-
 - "Executing" a DFA
 -}

getState :: (Ord s) => Dfa s t -> s -> State s t
getState dfa s = dfaStates dfa Map.! s

transition :: (Ord s, Ord t) => Dfa s t -> s -> t -> s
transition dfa s t =
  let state = getState dfa s
  in  case transitions state Map.!? t of
        (Just nextState) -> nextState
        Nothing          -> defaultTransition state

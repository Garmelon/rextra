{-# LANGUAGE ScopedTypeVariables #-}

module Rextra.Automaton
  ( dfaToNfa
  , nfaToDfa
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Tuple

import qualified Rextra.Dfa as Dfa
import           Rextra.Fa
import qualified Rextra.Nfa as Nfa
import           Rextra.Util

{-
 - Converting a DFA to a NFA
 -}

dfaStateToNfaState :: (Ord s, Ord t) => Dfa.State s t -> Nfa.State s t
dfaStateToNfaState s =
  let specialTokens = Map.keysSet $ Dfa.transitions s
      defaultTransition = (Nfa.AllExcept specialTokens, Dfa.defaultTransition s)
      otherTransitions = map (\(tokenSet, state) -> (Nfa.Only tokenSet, state))
                       $ map swap
                       $ Map.assocs
                       $ Dfa.transitionsByState s
  in  Nfa.State { Nfa.transitions        = defaultTransition : otherTransitions
                , Nfa.epsilonTransitions = Set.empty
                }

dfaToNfa :: (Ord s, Ord t) => Dfa.Dfa s t -> Nfa.Nfa s t
dfaToNfa a =
  let nfaStateMap = Map.map dfaStateToNfaState $ stateMap a
  -- The NFA was created from a valid DFA, so it will be valid too.
  in  fromJust $ fa nfaStateMap (entryState a) (exitStates a)

{-
 - Converting a NFA to a DFA
 -}

specialTokensOf :: Nfa.TransitionCondition t -> Set.Set t
specialTokensOf (Nfa.Only      t) = t
specialTokensOf (Nfa.AllExcept t) = t

-- | @'allSpecialTokens' a ns@ returns all tokens that behave
-- different from the default when when in state @ns@ of the automaton
-- @a@ (the default being an implicitly defined token that is not
-- mentioned in any of the automaton's state transitions).
specialTokensAt :: (Ord s, Ord t) => Nfa.Nfa s t -> Nfa.NdState s -> Set.Set t
specialTokensAt a ns =
  let ndStates = Nfa.getNdState a $ Nfa.epsilonStep a ns
  in  foldMap (foldMap (specialTokensOf . fst) . Nfa.transitions) ndStates

possibleTransitionsFrom :: (Ord s, Ord t)
                        => Nfa.Nfa s t -> Nfa.NdState s -> Map.Map t (Nfa.NdState s)
possibleTransitionsFrom a ns = Map.fromSet (transition a ns) (specialTokensAt a ns)

ndStateToDfaState :: (Ord s, Ord t) => Nfa.Nfa s t -> Nfa.NdState s -> Dfa.State (Nfa.NdState s) t
ndStateToDfaState a ns =
  Dfa.State { Dfa.transitions       = possibleTransitionsFrom a ns
            , Dfa.defaultTransition = Nfa.defaultTransition a ns
            }

nextStatesFrom :: (Ord s, Ord t) => Nfa.Nfa s t -> Nfa.NdState s -> Set.Set (Nfa.NdState s)
nextStatesFrom a ns =
  let tokenTransitioned   = Map.elems $ possibleTransitionsFrom a ns
      defaultTransitioned = Nfa.defaultTransition a ns
  in  Set.fromList $ defaultTransitioned : tokenTransitioned

-- This whole forall business is just so I can tell the Execute
-- typeclass that I mean Nfa.NdState when I say startState a.
connectedStates :: forall s t. (Ord s, Ord t) => Nfa.Nfa s t -> Set.Set (Nfa.NdState s)
connectedStates a =
  let start = Set.singleton (startState a :: Nfa.NdState s)
  in  connectedElements (nextStatesFrom a) start

dfaStateMap :: (Ord s, Ord t)
            => Nfa.Nfa s t -> Map.Map (Nfa.NdState s) (Dfa.State (Nfa.NdState s) t)
dfaStateMap a = Map.fromSet (ndStateToDfaState a) $ connectedStates a

nfaToDfa :: (Ord s, Ord t) => Nfa.Nfa s t -> Dfa.Dfa (Nfa.NdState s) t
nfaToDfa a =
  let theStateMap     = dfaStateMap a
      acceptingStates = Set.filter (Nfa.isAccepting a) $ Map.keysSet theStateMap
  in  fromJust $ fa theStateMap (startState a) acceptingStates

module Rextra.Automaton
  ( dfaToNfa
  , nfaToDfa
  ) where

import           Control.Monad.Trans.State
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Tuple
import qualified Rextra.Dfa as Dfa
import qualified Rextra.Nfa as Nfa

{-
 - Converting a DFA to a NFA
 -}

fromMonoidalList :: (Monoid m, Ord k) => [(k, m)] -> Map.Map k m
fromMonoidalList = foldl' insertMonoidal Map.empty
  where
    insertMonoidal :: (Monoid m, Ord k) => Map.Map k m -> (k, m) -> Map.Map k m
    insertMonoidal map (k, m) = Map.insertWith mappend k m map

groupByFirst :: (Ord a, Ord b) => [(a, b)] -> [(a, Set.Set b)]
groupByFirst pairs =
  let prepared = map (\(a, b) -> (a, Set.singleton b)) pairs
  in  Map.assocs $ fromMonoidalList prepared

dfaStateToNfaState :: (Ord s, Ord t) => Dfa.State s t -> Nfa.State s t
dfaStateToNfaState s =
  let transitionMap = Dfa.transitions s
      specialTokens = Map.keysSet transitionMap
      defaultTransition = (Nfa.AllExcept specialTokens, Dfa.defaultTransition s)
      otherTransitions = map (\(tSet, s) -> (Nfa.Only tSet, s))
                       . map swap
                       . groupByFirst
                       . map swap
                       $ Map.assocs transitionMap
  in  defaultTransition : otherTransitions

dfaToNfa :: (Ord s, Ord t) => Dfa.Dfa s t -> Nfa.Nfa s t
dfaToNfa dfa =
  let stateMap = Dfa.stateMap dfa
      exitingStates = map fst . filter (\(s, state) -> Dfa.accepting state) $ Map.assocs stateMap
      nfaStateMap = Map.map dfaStateToNfaState stateMap
  -- The NFA was created from a valid DFA, so it will be valid too.
  in  fromJust $ Nfa.nfa nfaStateMap (Dfa.entryState dfa) (Set.fromList exitingStates)

{-
 - Converting a NFA to a DFA
 -}

allSpecialTokens :: (Ord t) => [Nfa.State s t] -> Set.Set t
allSpecialTokens = foldMap (foldMap (Nfa.specialTokens . fst))

allNextStates :: (Ord s) => Dfa.State s t -> Set.Set s
allNextStates s =
  let nextStates = Map.elems $ Dfa.transitions s
  in  Set.fromList (Dfa.defaultTransition s : nextStates)

ndStateToDfaState :: (Ord s, Ord t) => Nfa.Nfa s t -> Nfa.NdState s -> Dfa.State (Nfa.NdState s) t
ndStateToDfaState nfa ns =
  let specialTokens = allSpecialTokens $ Nfa.getNdState nfa ns
  in  Dfa.State { Dfa.transitions       = Map.fromSet (Nfa.transition nfa ns) specialTokens
                , Dfa.defaultTransition = Nfa.defaultTransition nfa ns
                , Dfa.accepting         = Nfa.accepting nfa ns
                }

type Visited s = Set.Set (Nfa.NdState s)

exploreState :: (Ord s, Ord t)
             => Nfa.Nfa s t
             -> Nfa.NdState s
             -> State (Visited s) (Dfa.StateMap (Nfa.NdState s) t)
exploreState nfa ns = do
  visitedStates <- get
  if ns `Set.member` visitedStates
    then pure Map.empty
    else do
      modify (Set.insert ns) -- Adding this state to the visited states
      let dfaState    = ndStateToDfaState nfa ns
          ownStateMap = Map.singleton ns dfaState
          nextStates  = Set.toList $ allNextStates dfaState
      otherStateMaps <- mapM (exploreState nfa) nextStates
      pure $ Map.unions (ownStateMap : otherStateMaps)

dfaStateMap :: (Ord s, Ord t) => Nfa.Nfa s t -> Dfa.StateMap (Nfa.NdState s) t
dfaStateMap nfa = evalState (exploreState nfa (Nfa.entryNdState nfa)) Set.empty

nfaToDfa :: (Ord s, Ord t) => Nfa.Nfa s t -> Dfa.Dfa (Nfa.NdState s) t
nfaToDfa nfa = fromJust $ Dfa.dfa (dfaStateMap nfa) (Nfa.entryNdState nfa)

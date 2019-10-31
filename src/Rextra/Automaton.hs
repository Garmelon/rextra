{-# LANGUAGE ScopedTypeVariables #-}

module Rextra.Automaton
  ( dfaToNfa
  , nfaToDfa
  , minimizeDfa
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
  let start = Set.singleton $ Nfa.epsilonStep a $ (startState a :: Nfa.NdState s)
  in  connectedElements (nextStatesFrom a) start

dfaStateMap :: (Ord s, Ord t)
            => Nfa.Nfa s t -> Map.Map (Nfa.NdState s) (Dfa.State (Nfa.NdState s) t)
dfaStateMap a = Map.fromSet (ndStateToDfaState a) $ connectedStates a

nfaToDfa :: (Ord s, Ord t) => Nfa.Nfa s t -> Dfa.Dfa (Nfa.NdState s) t
nfaToDfa a =
  let theStateMap     = dfaStateMap a
      acceptingStates = Set.filter (Nfa.isAccepting a) $ Map.keysSet theStateMap
      theStartState   = Nfa.epsilonStep a $ startState a
  in  fromJust $ fa theStateMap theStartState acceptingStates

{-
 - Minimizing a DFA
 -}

type EquivalenceGroup s = Set.Set s
type Partition s = Set.Set (EquivalenceGroup s)
type Behaviour s t = Dfa.State (EquivalenceGroup s) t

partitionToMap :: (Ord s) => Partition s -> Map.Map s (EquivalenceGroup s)
partitionToMap partition = Map.fromList $ concatMap stateGroupAssocs $ Set.toList partition
  where stateGroupAssocs group = map (\s -> (s, group)) $ Set.toList group

stateToBehaviour :: (Ord s) => Map.Map s (EquivalenceGroup s) -> Dfa.State s t -> Behaviour s t
stateToBehaviour mapping = Dfa.normalize . Dfa.mapState (mapping Map.!)

findBehaviours :: (Ord s)
               => Map.Map s (EquivalenceGroup s)
               -> Map.Map s (Dfa.State s t)
               -> Map.Map s (Behaviour s t)
findBehaviours mapping statemap = Map.map (stateToBehaviour mapping) statemap

groupByBehaviour :: (Ord s, Ord t)
                 => Map.Map s (Behaviour s t)
                 -> EquivalenceGroup s
                 -> Map.Map (Behaviour s t) (EquivalenceGroup s)
groupByBehaviour mapping = groupByFirst . map (\s -> (mapping Map.! s, s)) . Set.toList

groupAllByBehaviour :: (Ord s, Ord t)
                    => Map.Map s (Behaviour s t)
                    -> Partition s
                    -> Map.Map (Behaviour s t) (EquivalenceGroup s)
groupAllByBehaviour mapping = Map.unions . map (groupByBehaviour mapping) . Set.toList

findNewBehaviourGrouping :: (Ord s, Ord t)
                         => Map.Map s (Dfa.State s t)
                         -> Partition s
                         -> Map.Map (Behaviour s t) (EquivalenceGroup s)
findNewBehaviourGrouping statemap partition =
  let mapping    = partitionToMap partition
      behaviours = findBehaviours mapping statemap
  in  groupAllByBehaviour behaviours partition

groupingToPartition :: (Ord s) => Map.Map (Behaviour s t) (EquivalenceGroup s) -> Partition s
groupingToPartition = Set.fromList . Map.elems

findGroupingFixpoint :: (Ord s, Ord t)
                     => Map.Map s (Dfa.State s t)
                     -> Partition s
                     -> Map.Map (Behaviour s t) (EquivalenceGroup s)
findGroupingFixpoint statemap partition =
  let newGrouping  = findNewBehaviourGrouping statemap partition
      newPartition = groupingToPartition newGrouping
  in  if partition == newPartition
      then newGrouping
      else findGroupingFixpoint statemap newPartition

initialPartition :: (Ord s) => Dfa.Dfa s t -> Partition s
initialPartition a =
  let (x, y) = Set.partition (a `accepts`) (states a)
  in  Set.fromList [x, y]

minimizeDfa :: (Ord s, Ord t) => Dfa.Dfa s t -> Dfa.Dfa (EquivalenceGroup s) t
minimizeDfa a =
  let grouping      = findGroupingFixpoint (stateMap a) (initialPartition a)
      mapping       = partitionToMap $ groupingToPartition grouping
      newStateMap   = Map.fromList $ map swap $ Map.assocs grouping
      newEntryState = mapping Map.! entryState a
      newExitStates = Set.map (mapping Map.!) (exitStates a)
  in  fromJust $ fa newStateMap newEntryState newExitStates

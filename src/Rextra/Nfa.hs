{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Rextra.Nfa
  ( Nfa
  , only
  , allExcept
  , nfa
  , State(..)
  , TransitionCondition(..)
  , holdsTrueFor
  , holdsTrueForDefault
  -- * Executing
  , NdState
  , isAccepting
  , getNdState
  , epsilonStep
  , defaultTransition
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Rextra.Fa
import           Rextra.Util

-- State stuff

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

-- | Whether the condition holds true for a token.
holdsTrueFor :: (Ord t) => TransitionCondition t -> t -> Bool
holdsTrueFor (Only s)      t = Set.member t s
holdsTrueFor (AllExcept s) t = Set.notMember t s

holdsTrueForDefault :: TransitionCondition t -> Bool
holdsTrueForDefault (AllExcept _) = True
holdsTrueForDefault _             = False

data State s t = State
  { transitions        :: [(TransitionCondition t, s)]
  , epsilonTransitions :: Set.Set s
  } deriving (Show)

instance FaState State where
  canReach State{transitions, epsilonTransitions} =
    Set.union epsilonTransitions $ Set.fromList $ map snd transitions

-- Nfa stuff

type Nfa s t = Fa State s t
type NdState s = Set.Set s

-- Not the most brilliant of names, very similar to Executable's
-- 'accepts'. But I'd rather repeat myself in this way than
-- reimplement this predicate everywhere I need it (e. g. in
-- 'nfaToDfa' in Automaton.hs).
isAccepting :: (Ord s) => Nfa s t -> NdState s -> Bool
isAccepting a ns = not $ ns `Set.disjoint` exitStates a

instance (Ord s) => Executable (Fa State s) (Set.Set s) where
  startState = Set.singleton . entryState
  transition = nfaTransition
  accepts    = isAccepting

only :: (Ord t) => [t] -> TransitionCondition t
only = Only . Set.fromList

allExcept :: (Ord t) => [t] -> TransitionCondition t
allExcept = AllExcept . Set.fromList

nfa :: (Ord s)
     => [(s, [(TransitionCondition t, s)], [s])]
     -> s
     -> [s]
     -> Maybe (Nfa s t)
nfa stateInfo entryState exitStates =
  let stateList = map (\(s, ts, et) -> (s, State ts (Set.fromList et))) stateInfo
  in  fa (Map.fromList stateList) entryState (Set.fromList exitStates)

-- Transitions

getNdState :: (Ord s) => Nfa s t -> NdState s -> [State s t]
getNdState a ns = map (getState a) $ Set.toList ns

epsilonStep :: (Ord s) => Nfa s t -> NdState s -> NdState s
epsilonStep a ns = connectedElements (epsilonTransitions . getState a) ns

tokenStep :: (Ord s, Ord t) => Nfa s t -> t -> NdState s -> NdState s
tokenStep a t ns = foldMap (nextStates t) $ getNdState a ns
  where
    nextStates :: (Ord s, Ord t) => t -> State s t -> Set.Set s
    nextStates token state = Set.fromList
                       $ map snd
                       $ filter (\(cond, _) -> cond `holdsTrueFor` token)
                       $ transitions state

defaultStep :: (Ord s) => Nfa s t -> NdState s -> NdState s
defaultStep a ns = Set.fromList
                 $ map snd
                 $ filter (holdsTrueForDefault . fst)
                 $ concatMap transitions
                 $ getNdState a ns

nfaTransition :: (Ord s, Ord t) => Nfa s t -> NdState s -> t -> NdState s
nfaTransition a s t = epsilonStep a $ tokenStep a t $ epsilonStep a s

defaultTransition :: (Ord s) => Nfa s t -> NdState s -> NdState s
defaultTransition a = epsilonStep a . defaultStep a . epsilonStep a

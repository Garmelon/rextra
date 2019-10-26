{-# LANGUAGE TupleSections #-}

module Rextra.Visualize where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Rextra.Dfa as Dfa
import qualified Rextra.Nfa as Nfa
import           Rextra.Util

{-
 - Visualizing DFAs
 -}

convertDfaState :: (Int, Dfa.State Int Char) -> [LEdge String]
convertDfaState (from, state) =
  let normalEdges = map (\(t, to) -> (from, to, [t])) . Map.assocs $ Dfa.transitions state
      defaultEdge = (from, Dfa.defaultTransition state, "default")
  in  defaultEdge : normalEdges

dfaToGraph :: Dfa.Dfa Int Char -> Gr () String
dfaToGraph dfa =
  let stateMap = Dfa.stateMap dfa
      nodes    = map (\k -> (k, ())) $ Map.keys stateMap
      edges    = concatMap convertDfaState $ Map.assocs stateMap
  in  mkGraph nodes edges

dfaAttributes :: Labellable el => Dfa.Dfa Int t -> GraphvizParams Int nl el () nl
dfaAttributes dfa =
  let exitStates = Dfa.exitStates dfa
      fmtNode (n, l) = if n `Set.member` exitStates
        then [shape DoubleCircle]
        else [shape Circle]
      fmtEdge (n1, n2, l) = [toLabel l]
  in  nonClusteredParams { fmtNode = fmtNode, fmtEdge = fmtEdge }

showDfa :: Dfa.Dfa Int Char -> IO ()
showDfa dfa = runGraphvizCanvas' (graphToDot (dfaAttributes dfa) (dfaToGraph dfa)) Gtk

{-
 - Visualizing NFAs
 -}

labelFromSet :: Set.Set Char -> String
labelFromSet charSet = "{" ++ intersperse ',' (Set.toList charSet) ++ "}"

labelFromCondition :: Nfa.TransitionCondition Char -> String
labelFromCondition (Nfa.Only charSet)      = labelFromSet charSet
labelFromCondition (Nfa.AllExcept charSet)
  | Set.null charSet = "Σ"
  | otherwise        = "Σ\\" ++ labelFromSet charSet

convertNfaState :: (Int, Nfa.State Int Char) -> [LEdge String]
convertNfaState (from, state) =
  let transitions        = Nfa.transitions state
      transitionEdges    = map (\(cond, to) -> (from, to, labelFromCondition cond)) transitions
      epsilonTransitions = Set.toList $ Nfa.epsilonTransitions state
      epsilonEdges       = map (\to -> (from, to, "ε")) epsilonTransitions
  in transitionEdges ++ epsilonEdges

nfaToGraph :: Nfa.Nfa Int Char -> Gr () String
nfaToGraph nfa =
  let stateMap = Nfa.stateMap nfa
      nodes    = map (\k -> (k, ())) $ Map.keys stateMap
      edges    = concatMap convertNfaState $ Map.assocs stateMap
  in  mkGraph nodes edges

anyNfaToGraph :: (Ord s) => Nfa.Nfa s Char -> Gr () String
anyNfaToGraph = nfaToGraph . Nfa.rename

nfaAttributes :: Labellable el => Nfa.Nfa Int t -> GraphvizParams Int nl el () nl
nfaAttributes nfa =
  let exitStates = Nfa.exitStates nfa
      fmtNode (n, l) = if n `Set.member` exitStates
        then [shape DoubleCircle]
        else [shape Circle]
      fmtEdge (n1, n2, l) = [toLabel l]
  in  nonClusteredParams { fmtNode = fmtNode, fmtEdge = fmtEdge }

showNfa :: Nfa.Nfa Int Char -> IO ()
showNfa nfa = runGraphvizCanvas' (graphToDot (nfaAttributes nfa) (nfaToGraph nfa)) Gtk

{-# LANGUAGE TupleSections #-}

module Rextra.Visualize
  ( showDot
  , saveDot
  , saveDotAsPng
  , dfaToDot
  , nfaToDot
  ) where

import           Control.Monad
import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Rextra.Dfa as Dfa
import qualified Rextra.Nfa as Nfa
import           Rextra.Util

showDot :: DotGraph Node -> IO ()
showDot dg = runGraphvizCanvas' dg Gtk

saveDot :: GraphvizOutput -> String -> DotGraph Node -> IO ()
saveDot format path dg = void $ runGraphviz dg format path

saveDotAsPng :: String -> DotGraph Node -> IO ()
saveDotAsPng = saveDot Png

{-
 - Visualizing DFAs
 -}

convertDfaState :: (Int, Dfa.State Int Char) -> [LEdge String]
convertDfaState (from, state) =
  let normalEdges = map (\(t, to) -> (from, to, [t])) . Map.assocs $ Dfa.transitions state
      defaultEdge = (from, Dfa.defaultTransition state, "**")
  in  defaultEdge : normalEdges

-- | Convert a 'Dfa.Dfa' to a 'Gr' graph.
--
-- This function assumes that no node has a negative name.
dfaToGraph :: Dfa.Dfa Int Char -> Gr () String
dfaToGraph dfa =
  let stateMap  = Dfa.stateMap dfa
      startNode = (-1, ())
      startEdge = (-1, Dfa.entryState dfa, "")
      nodes     = map (\k -> (k, ())) $ Map.keys stateMap
      edges     = concatMap convertDfaState $ Map.assocs stateMap
  in  mkGraph (startNode : nodes) (startEdge : edges)

dfaAttributes :: Labellable el => Dfa.Dfa Int t -> GraphvizParams Int nl el () nl
dfaAttributes dfa =
  let exitStates = Dfa.exitStates dfa

      fmtNode (-1, _) = [style invis] -- Start node
      fmtNode ( n, _)
        | n `Set.member` exitStates = [shape DoubleCircle]
        | otherwise                 = [shape Circle]

      fmtEdge (-1, _, _) = [] -- Start edge
      fmtEdge ( _, _, l) = [toLabel l]

  in  nonClusteredParams { fmtNode = fmtNode, fmtEdge = fmtEdge }

dfaToDot :: Dfa.Dfa Int Char -> DotGraph Node
dfaToDot dfa = graphToDot (dfaAttributes dfa) (dfaToGraph dfa)

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
  let stateMap  = Nfa.stateMap nfa
      startNode = (-1, ())
      startEdge = (-1, Nfa.entryState nfa, "")
      nodes     = map (\k -> (k, ())) $ Map.keys stateMap
      edges     = concatMap convertNfaState $ Map.assocs stateMap
  in  mkGraph (startNode : nodes) (startEdge : edges)

anyNfaToGraph :: (Ord s) => Nfa.Nfa s Char -> Gr () String
anyNfaToGraph = nfaToGraph . Nfa.rename

nfaAttributes :: Labellable el => Nfa.Nfa Int t -> GraphvizParams Int nl el () nl
nfaAttributes nfa =
  let exitStates = Nfa.exitStates nfa

      fmtNode (-1, _) = [style invis] -- Start node
      fmtNode ( n, _)
        | n `Set.member` exitStates = [shape DoubleCircle]
        | otherwise                 = [shape Circle]

      fmtEdge (-1, _, _) = [] -- Start edge
      fmtEdge ( _, _, l) = [toLabel l]

  in  nonClusteredParams { fmtNode = fmtNode, fmtEdge = fmtEdge }

nfaToDot :: Nfa.Nfa Int Char -> DotGraph Node
nfaToDot nfa = graphToDot (nfaAttributes nfa) (nfaToGraph nfa)

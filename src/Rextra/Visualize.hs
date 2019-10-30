{-# LANGUAGE FlexibleInstances #-}

module Rextra.Visualize where

import           Control.Monad
import           Data.GraphViz
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Rextra.Dfa as Dfa
import           Rextra.Fa
import qualified Rextra.Nfa as Nfa

-- First, some labelling...

-- | Instances of this type class must fulfill the following rules:
--
-- * The return value of 'asLabel' must never be an empty string.
--
-- * @a == b@ is equivalent to @'asLabel' a == 'asLabel' b@.
class AsLabel a where
  asLabel :: a -> String

instance AsLabel Int where
  asLabel = show

instance AsLabel Char where
  asLabel c = [c]

-- | __Warning__: Only use this if you know that no empty strings
-- occur in your automaton's state names!
instance AsLabel [Char] where
  asLabel = id

instance AsLabel a => AsLabel (Set.Set a) where
  asLabel s =
    let sublabels = map asLabel $ Set.toList s
    in  "{" ++ intercalate "," sublabels ++ "}"

{-
 - Displaying or saving 'DotGraph's
 -}

showDot :: (PrintDotRepr dg n) => dg n -> IO ()
showDot dg = runGraphvizCanvas' dg Gtk

saveDot :: (PrintDotRepr dg n) => GraphvizOutput -> String -> dg n -> IO ()
saveDot format path dg = void $ runGraphviz dg format path

saveDotAsPng :: (PrintDotRepr dg n) => String -> dg n -> IO ()
saveDotAsPng = saveDot Png

{-
 - General 'Fa' stuff
 -}

-- The node label is a visibility flag. If it is False the node should
-- be invisible in the displayed graph.
nodes :: (AsLabel s) => Fa state s t -> [(String, Bool)]
nodes a =
  let normalNodes = map (\s -> (asLabel s, True)) $ Set.toList $ states a
      entryNode   = ("", False)
  in entryNode : normalNodes

-- Finally, a good use for the list monad! I couldn't even have used
-- the applicative instance instead :D
edges :: (Ord s, AsLabel s)
      => Fa state s t -> (state s t -> [(String, s)]) -> [(String, String, String)]
edges a f = ("", asLabel $ entryState a, "") : do
  from        <- Set.toList $ states a
  (label, to) <- f $ getState a from
  pure (asLabel from, asLabel to, label)

faParams :: (AsLabel s) => Fa state s t -> GraphvizParams String Bool String () Bool
faParams a =
  let acceptingStates = Set.map asLabel $ exitStates a

      formatNode (_, False) = [style invis] -- See comment on 'nodes'
      formatNode (s,  True) =
        let accepting = s `Set.member` acceptingStates
        in  [toLabel s, shape (if accepting then DoubleCircle else Circle)]

      formatEdge (_, _,    "") = [] -- Probably the start edge
      formatEdge (_, _, label) = [toLabel label]
  in  nonClusteredParams { fmtNode = formatNode, fmtEdge = formatEdge }

faToDot :: (Ord s, AsLabel s)
        => (state s t -> [(String, s)]) -> Fa state s t -> DotGraph String
faToDot f a = graphElemsToDot (faParams a) (nodes a) (edges a f)

{-
 - 'Dfa' stuff
 -}

dfaShowNoDefault :: (Ord s, Ord t, AsLabel t) => Dfa.State s t -> [(String, s)]
dfaShowNoDefault = map (\(s, t) -> (asLabel t, s)) . Map.assocs . Dfa.transitionsByState

dfaShowAll :: (Ord s, Ord t, AsLabel t) => Dfa.State s t -> [(String, s)]
dfaShowAll s = ("*", Dfa.defaultTransition s) : dfaShowNoDefault s

dfaToDot :: (Ord s, AsLabel s, Ord t, AsLabel t) => Dfa.Dfa s t -> DotGraph String
dfaToDot = faToDot dfaShowAll

dfaToDotNoDefault :: (Ord s, AsLabel s, Ord t, AsLabel t) => Dfa.Dfa s t -> DotGraph String
dfaToDotNoDefault = faToDot dfaShowNoDefault

{-
 - 'Nfa' stuff
 -}

showCondition :: (AsLabel t) => Nfa.TransitionCondition t -> String
showCondition (Nfa.Only s)      = asLabel s
showCondition (Nfa.AllExcept s)
  | Set.null s = "Σ"
  | otherwise  = "Σ\\" ++ asLabel s

nfaShow :: (AsLabel t) => Nfa.State s t -> [(String, s)]
nfaShow state =
  let normalEdges  = map (\(cond, s) -> (showCondition cond, s)) $ Nfa.transitions state
      epsilonEdges = map (\s -> ("ε", s)) $ Set.toList $ Nfa.epsilonTransitions state
  in  normalEdges ++ epsilonEdges

nfaToDot :: (Ord s, AsLabel s, AsLabel t) => Nfa.Nfa s t -> DotGraph String
nfaToDot = faToDot nfaShow

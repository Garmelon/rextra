module Rextra.Util
  ( connectedElements
  ) where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Set as Set

explore :: (Ord n) => (n -> Set.Set n) -> n -> State (Set.Set n) ()
explore trans node = do
  visited <- get
  unless (node `Set.member` visited) $ do
    modify (Set.insert node)
    mapM_ (explore trans) . Set.toList $ trans node

connectedElements :: (Ord n) => (n -> Set.Set n) -> Set.Set n -> Set.Set n
connectedElements trans startingNodes =
  flip execState Set.empty . mapM (explore trans) $ Set.toList startingNodes

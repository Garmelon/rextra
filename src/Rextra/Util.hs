{-# LANGUAGE TupleSections #-}

module Rextra.Util
  ( connectedElements
  -- * Renaming
  , Rename
  , doRename
  , getName
  , renameSet
  , renameMap
  , renameKeys
  , renameValues
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

type Rename n = State (Int, Map.Map n Int)

doRename :: Rename n a -> a
doRename rename = evalState rename (0, Map.empty)

getName :: (Ord n) => n -> Rename n Int
getName thing = do
  (i, names) <- get
  case names Map.!? thing of
    Just name -> pure name
    Nothing   -> i <$ put (i + 1, Map.insert thing i names)

renameSet :: (Ord v2) => (v1 -> Rename n v2) -> Set.Set v1 -> Rename n (Set.Set v2)
renameSet renameFunc s = Set.fromList <$> (mapM renameFunc $ Set.toList s)

renameMap :: (Ord k2)
          => ((k1, v1) -> Rename n (k2, v2))
          -> Map.Map k1 v1
          -> Rename n (Map.Map k2 v2)
renameMap f m = Map.fromList <$> (mapM f $ Map.assocs m)

renameKeys :: (Ord k2) => (k1 -> Rename n k2) -> Map.Map k1 v -> Rename n (Map.Map k2 v)
renameKeys f = renameMap (\(k, v) -> (,v) <$> f k)

renameValues :: (Ord k) => (v1 -> Rename n v2) -> Map.Map k v1 -> Rename n (Map.Map k v2)
renameValues f = renameMap (\(k, v) -> (k,) <$> f v)

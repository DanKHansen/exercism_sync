{-# LANGUAGE LambdaCase #-}

module BST (BST, bstLeft, bstRight, bstValue, empty, fromList, insert, singleton, toList) where

data BST a = Empty | Node (BST a) a (BST a) deriving (Eq, Show)

empty :: BST a
empty = Empty

singleton :: a -> BST a
singleton x = Node Empty x Empty

insert :: Ord a => a -> BST a -> BST a
insert x = \case
  Empty -> singleton x
  Node l v r ->
    if x <= v
      then Node (insert x l) v r
      else Node l v (insert x r)

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

toList :: BST a -> [a]
toList = \case
  Empty -> []
  Node l v r -> toList l ++ v : toList r

bstLeft :: BST a -> Maybe (BST a)
bstLeft n = case n of
  Node l _ _ -> Just l
  _ -> Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight n = case n of
  Node _ _ r -> Just r
  _ -> Nothing

bstValue :: BST a -> Maybe a
bstValue n = case n of
  Node _ v _ -> Just v
  _ -> Nothing
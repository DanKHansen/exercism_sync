module BST (BST, bstLeft, bstRight, bstValue, empty, fromList, insert, singleton, toList) where

data BST a = Empty | Tree (BST a) a (BST a) deriving (Eq, Show)

empty :: BST a
empty = Empty

singleton :: a -> BST a
singleton x = Tree Empty x Empty

insert :: Ord a => a -> BST a -> BST a
insert x n = case n of
  Empty -> singleton x
  Tree l v r ->
    if x <= v then Tree (insert x l) v r else Tree l v (insert x r)

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

toList :: BST a -> [a]
toList n = case n of Empty -> []; Tree l v r -> toList l ++ v : toList r

bstLeft :: BST a -> Maybe (BST a)
bstLeft n = case n of Tree l _ _ -> Just l; _ -> Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight n = case n of Tree _ _ r -> Just r; _ -> Nothing

bstValue :: BST a -> Maybe a
bstValue n = case n of Tree _ v _ -> Just v; _ -> Nothing
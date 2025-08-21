module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

data BST a = Empty | Node {left :: BST a, value :: a, right :: BST a} deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft tree = Just $ left tree

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight tree = Just $ right tree

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue tree = Just $ value tree

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node l v r)
  | x <= v = Node (insert x l) v r
  | otherwise = Node l v (insert x r)

singleton :: a -> BST a
singleton x = Node Empty x Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node l v r) = toList l ++ [v] ++ toList r

{- I tried writing a version with parent references, but haskell isn't really a language for it. Atleast my Sum data type version of it is not suitable for this.
Abandoned, but left it as a curiosity.
-}
import Data.List (foldl')

data BST a = Empty
           | Leaf { val :: a, parent :: BST a }
           | Node { val :: a, left :: BST a, right :: BST a, parent :: BST a}
           deriving (Eq)

instance (Show a) => Show (BST a) where
  show Empty = "Empty"
  show (Leaf x _) = "L<" ++ show x ++ ">"
  show (Node x l r _) = "N<" ++ show x ++ ", [" ++ show l ++ "], [" ++ show r ++ "]>"
  
insert :: (Ord a) => BST a -> a -> BST a
insert Empty x = let p = Leaf x p in p
insert t@(Leaf y p) x = case compare x y of
                        EQ -> t
                        LT -> let np = Node y (Leaf x np) Empty p in
                                np
                        GT -> let np = Node y Empty (Leaf x np) p in
                                np
insert t@(Node y l r p) x = case compare x y of
                              EQ -> t
                              LT -> Node y (insert l x) r p
                              GT -> Node y l (insert r x) p

create :: (Ord a) => [a] -> BST a
create = foldl' insert Empty

inorder :: BST a -> [a]
inorder Empty = []
inorder (Leaf x _) = [x]
inorder (Node y l r _) = inorder l ++ (y:inorder r)

bsort :: (Ord a) => [a] -> [a]
bsort = inorder . create

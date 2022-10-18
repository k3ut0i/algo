import Data.List (nub, sort)
import Test.QuickCheck

bsort :: [Int] -> [Int]
bsort = inOrder . foldl insertBTree Nil

main :: IO ()
main = verboseCheck (\xs -> sort (nub xs) == bsort xs)
data BTree a = Nil | BTree a (BTree a) (BTree a)
  deriving(Show, Eq)

insertBTree :: (Ord a) => BTree a -> a -> BTree a
insertBTree Nil x = BTree x Nil Nil
insertBTree t@(BTree y l r) x = case compare x y of
                                  EQ -> t
                                  LT -> BTree y (insertBTree l x) r
                                  GT -> BTree y l (insertBTree r x)

inOrder :: BTree a -> [a]
inOrder Nil = []
inOrder (BTree n l r) = inOrder l ++ n : (inOrder r)

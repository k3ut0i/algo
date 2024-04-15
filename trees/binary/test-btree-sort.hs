import Data.List (nub, sort, foldl')
import Test.QuickCheck
import Data.Maybe (fromJust)

main :: IO ()
main = quickCheck bsortTest >>
       quickCheck testMinMax >>
       testPP >>
       quickCheck testPred >>
       quickCheck testSucc >>
       quickCheck testDelete
data BTree a = Nil | BTree { val :: a
                           , left :: (BTree a)
                           , right :: (BTree a)
                           }
  deriving(Show, Eq)

insertBTree :: (Ord a) => BTree a -> a -> BTree a
insertBTree Nil x = BTree x Nil Nil
insertBTree t@(BTree y l r) x = case compare x y of
                                  LT -> BTree y (insertBTree l x) r 
                                  GT -> BTree y l (insertBTree r x)
                                  EQ -> t

createBTree :: (Ord a) => [a] -> BTree a
createBTree = foldl' insertBTree Nil

inOrder :: BTree a -> [a]
inOrder Nil = []
inOrder (BTree n l r) = inOrder l ++ n : (inOrder r)
findBTree :: (Ord a) => a -> BTree a -> BTree a
findBTree x Nil = Nil
findBTree x t@(BTree y l r) = case compare x y of
                                EQ -> t
                                LT -> findBTree x l
                                GT -> findBTree x r

findPath :: (Ord a) => a -> BTree a -> Maybe (BTree a, [(Bool, BTree a)])
findPath x t = f x t []
  where f x Nil acc = Nothing
        f x t@(BTree y l r) acc = case compare x y of
                                    EQ -> Just (t, acc)
                                    LT -> f x l ((False, t):acc)
                                    GT -> f x r ((True, t):acc)


bsort :: [Int] -> [Int]
bsort = inOrder . createBTree

bsortTest :: [Int] -> Bool
bsortTest xs = sort (nub xs) == bsort xs
minBTree :: (Ord a) => BTree a -> Maybe (BTree a)
minBTree Nil = Nothing
minBTree t@(BTree y Nil _) = Just t
minBTree (BTree y t@(BTree _ _ _) _) = minBTree t

maxBTree :: (Ord a) => BTree a -> Maybe (BTree a)
maxBTree Nil = Nothing
maxBTree t@(BTree y _ Nil) = Just t
maxBTree (BTree y _ t@(BTree _ _ _)) = maxBTree t

minB t = val <$> minBTree t
maxB t = val <$> maxBTree t
testMinMax :: [Int] -> Bool
testMinMax xs = let mi = if xs == [] then Nothing else Just (minimum xs)
                    ma = if xs == [] then Nothing else Just (maximum xs)
                    t = createBTree xs
                in
                  mi == (minB t) && ma == (maxB t)
pshow :: (Show a) => BTree a -> String
pshow = f 0
  where space i = take i (repeat ' ')
        f i Nil =  space i ++ "*\n"
        f i (BTree x l r) = space i ++ (show x) ++ "\n" ++
                            f (i+1) l ++
                            f (i+1) r
testPP :: IO ()
testPP = putStr . pshow . createBTree $ [4, 2, 3, 1, 8, 7, 9, 0, 5]
succBTree :: (Ord a) => a -> BTree a -> Maybe (BTree a)
succBTree x t = case findPath x t of
                  Nothing -> Nothing
                  Just (BTree x l r, ps) -> case r of
                                              Nil -> leftparent ps
                                              (BTree _ _ _) -> minBTree r
  where leftparent :: [(Bool, BTree a)] -> Maybe (BTree a)
        leftparent [] = Nothing
        leftparent ((True, p):ps) = leftparent ps
        leftparent ((False, t@(BTree x _ _)):_) = Just t
             

predBTree :: (Ord a) => a -> BTree a -> Maybe (BTree a)
predBTree x t = case findPath x t of
                  Nothing -> Nothing
                  Just (BTree x l r, ps) -> case l of
                                              Nil -> rightparent ps
                                              (BTree _ _ _) -> maxBTree l

  where rightparent :: [(Bool, BTree a)] -> Maybe (BTree a)
        rightparent [] = Nothing
        rightparent ((False, _):ps) = rightparent ps
        rightparent ((True, t@(BTree x _ _)):_) = Just t

succB x t = val <$> succBTree x t
predB x t = val <$> predBTree x t
testPred :: [Int] -> Bool
testPred xs = let t = createBTree xs
                  s = reverse . sort . nub $ xs
              in
                case s of
                  [] -> True
                  [_] -> True
                  ss -> f (\x -> predB x t) ss

testSucc :: [Int] -> Bool
testSucc xs = let t = createBTree xs
                  s = sort (nub xs)
              in
                case s of
                  [] -> True
                  [_] -> True
                  ss -> f (\x -> succB x t) ss

f g [] = True
f g [x] = g x == Nothing
f g (x:y:xs) = (g x == Just y) && f g (y:xs)
delete :: (Ord a) => BTree a -> a -> BTree a
delete Nil x = Nil
delete t@(BTree y Nil Nil) x | x == y = Nil
                             | x /= y = t
delete t@(BTree y Nil r) x = case compare x y of
                               EQ -> r
                               LT -> t
                               GT -> BTree y Nil (delete r x)
delete t@(BTree y l Nil) x = case compare x y of
                               EQ -> l
                               LT -> BTree y (delete l x) Nil
                               GT -> t
delete t@(BTree y l r) x = case compare x y of
                             LT -> BTree y (delete l x) r
                             GT -> BTree y l (delete r x)
                             EQ -> case succBTree x t of
                                     Just (BTree x' Nil r') -> BTree x' l (replace x' r' r)
                                     Nothing -> error "right subtree should exist."
                                     

replace :: (Ord a) => a -> BTree a -> BTree a -> BTree a
replace x t1 Nil = Nil
replace x t1 t@(BTree y l r) = case compare x y of
                                 EQ -> t1
                                 LT -> BTree y (replace x t1 l) r
                                 GT -> BTree y l (replace x t1 r)
testDelete :: [Int] -> Bool
testDelete xs = let ss = sort (nub xs)
                    t = createBTree xs
                    f Nil [] = True
                    f (BTree _ _ _) [] = False
                    f Nil (_:_) = False
                    f t s@(x:xs) = (inOrder t == s) && f (delete t x) xs
                in
                  f t ss

import Data.Array
import Data.List (sort, unfoldr)
import Test.QuickCheck
data PQ a = PQ { dataArray :: Array Int a
               , size :: Int }
mkPQ :: Int -> a -> PQ a
mkPQ size e = PQ (listArray (0, size-1) $ repeat e) 0

--mkPQdef :: (Default a) => Int -> PQ a
--mkPQdef size = PQ (listArray (0, size-1) $ repeat def) 0

parent i = div (i-1) 2
childL i = 2*i+1
childR i = 2*i+2
upHeap :: (Ord a) => Array Int a -> Int -> Array Int a
upHeap a 0 = a
upHeap a i = let ip = parent i; e = a!i; ep = a!ip in
               if e > ep
               then upHeap (a//[(i, ep), (ip, e)]) ip
               else a

downHeap :: (Ord a) => Array Int a -> Int -> Int -> Array Int a
downHeap a s i = let il = childL i
                     ir = childR i
                     mi = foldl (f a s) i [il, ir]
                 in
                   if mi == i
                   then a
                   else downHeap (a//[(i, a!mi), (mi, a!i)]) s mi
  where f a s im i | i >= s = im
                   | a!i < a!im = im
                   | otherwise = i
insertPQ :: (Ord a) => PQ a -> a -> PQ a
insertPQ (PQ a s) e = let an = a//[(s, e)] in
                        PQ (upHeap an s) (s+1)

extractPQ :: (Ord a) => PQ a -> Maybe (a, PQ a)
extractPQ (PQ _ 0) = Nothing
extractPQ (PQ a s) = let top = a!0
                         sn  = s-1
                         an  = a//[(0, a!sn)]
                     in
                       Just (top, PQ (downHeap an sn 0) sn)
heapSort :: (Ord a) => [a] -> [a]
heapSort [] = []
heapSort xs@(x:_) = let len = length xs
                        pq = foldl insertPQ (mkPQ len x) xs
                    in unfoldr extractPQ pq
main =
  -- verboseCheck testHeap
  quickCheck testHeap

testHeap :: [Int] -> Bool
testHeap xs = heapSort xs == reverse (sort xs)

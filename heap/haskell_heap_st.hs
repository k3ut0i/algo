import Data.Array.ST
import Data.List (sort)
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Test.QuickCheck
data PQ s a = PQ { dataArray :: STArray s Int a, size :: Int}
mkPQ :: Int -> a -> ST s (PQ s a)
mkPQ size e = newArray (0, size-1) e >>= return . flip PQ 0

parent i = div (i-1) 2
childL i = 2*i+1
childR i = 2*i+2
upHeap :: (Ord a) => STArray s Int a -> Int -> ST s (STArray s Int a)
upHeap a 0 = return a
upHeap a i = readArray a i >>=
             \e -> readArray a (parent i) >>=
             \ep -> if e > ep
                    then writeArray a i ep >>
                         writeArray a (parent i) e >>
                         upHeap a (parent i)
                    else return a

downHeap :: (Ord a) => STArray s Int a -> Int -> Int -> ST s (STArray s Int a)
downHeap a s i = let il = childL i
                     ir = childR i
                 in
                   foldM (f a s) i [il, ir] >>=
                   \mi -> if mi == i
                          then return a
                          else readArray a i >>=
                               \e -> readArray a mi >>=
                               \me -> writeArray a i me >>
                                      writeArray a mi e >>
                                      downHeap a s mi
  where f a s im i | i >= s = return im
                   | otherwise = readArray a im >>=
                                 \em -> readArray a i >>=
                                 \e -> return $ if (e > em) then i else im
insertPQ :: (Ord a) => PQ s a -> a -> ST s (PQ s a)
insertPQ (PQ a s) e = writeArray a s e >> upHeap a s >>= return . flip PQ (s+1)

extractPQ :: (Ord a) => PQ s a -> ST s (Maybe (a, PQ s a))
extractPQ (PQ a 0) = return Nothing
extractPQ (PQ a s) = readArray a 0 >>=
                     \top -> readArray a (s-1)  >>=
                     \laste -> writeArray a 0 laste >>
                     downHeap a (s-1) 0 >>= \an -> return (Just (top, PQ an (s-1)))
heapSort :: (Ord a) => [a] -> [a]
heapSort [] = []
heapSort xs = runST $ let len = length xs in
                        mkPQ len (head xs) >>=
                        \pq -> foldM insertPQ pq xs >>=
                        unfoldM extractPQ
  where unfoldM f x = f x >>= \a -> case a of
                                      Nothing -> return []
                                      Just(e, x1) -> (e:) <$> unfoldM f x1
main =
  -- verboseCheck testHeap
  quickCheck testHeap

testHeap :: [Int] -> Bool
testHeap xs = heapSort xs == reverse (sort xs)

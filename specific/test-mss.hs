import Data.Array
import Test.QuickCheck

main = quickCheck mSSprop

mSSprop :: [Int] -> Bool
mSSprop [] = True
mSSprop xs = let a = listArray (1, length xs) xs in
               mSS a == max 0 (mSSBrute a)
mSS :: (Integral a) => Array Int a -> a
mSS a = m' a (max 0 (a!1)) (max 0 (a!1)) 2
  where m' a best curr i | i > snd (bounds a) = best
                         | otherwise = let c = curr + (a!i) in
                             m' a (max c best) (max c 0) (i+1)
mSSBrute :: (Integral a) => Array Int a -> a
mSSBrute a =
  let (1, n) = bounds a
      sumA x i j = foldl (\acc i -> acc + x!i) 0 [i..j]
  in
    maximum . map (uncurry (sumA a)) $ [(i, j) | i <- [1..n], j <- [1..n]]

module Data.List.TakeR where

import qualified Data.Vector.Mutable as V
import Control.Monad.ST
import Debug.Trace

-- Efficient taking of last r values
takeR :: Int -> [a] -> [a]
takeR n l | n <= 0 = []
takeR n l = runST $ do
    buffer <- V.new n
    i <- go buffer 0 l
    let s = min i n
    sequence $ [ V.read buffer (j `mod` n) | j <- [i-s..i-1] ]
  where
    go buffer i [] = return i
    go buffer i (x:xs) = V.write buffer (i `mod` n) x >> go buffer (i+1) xs

    
-- Correctness asserted by
-- quickCheck (\n l -> n <= 100000 ==> takeR n l == reverse (take n (reverse (l::[Int]) )))

{-# LANGUAGE ScopedTypeVariables #-}
module Data.List.TakeR where

-- Efficient taking of last r values
takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
  where
    go [] r = r
    go (x:xs) (y:ys) = go xs ys

-- Much faster and better evaluation properties than:
{-
import Control.Monad.ST
import Debug.Trace
import Data.Array.MArray
import Data.Array.ST

takeR :: forall a. Int -> [a] -> [a]
takeR n l | n <= 0 = []
takeR n l = runST stAction
  where
    stAction :: forall s. ST s [a]
    stAction = do
        buffer <- newArray_ (0, n-1)
        i <- go (buffer :: STArray s Int a) 0 l
        let s = min i n
        sequence $ [ readArray buffer (j `mod` n) | j <- [i-s..i-1] ]
    go buffer i [] = return i
    go buffer i (x:xs) = writeArray buffer (i `mod` n) x >> go buffer (i+1) xs
-}

    
-- Correctness asserted by
-- quickCheck (\n l -> n <= 100000 ==> takeR n l == reverse (take n (reverse (l::[Int]) )))

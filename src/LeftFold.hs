{-# LANGUAGE ExistentialQuantification, TypeOperators #-}

module LeftFold where

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Strict ((:!:), Pair((:!:)))
import qualified Data.Strict as S


data LeftFold x a = forall s. LeftFold {
    start :: s,
    process :: s -> x -> s,
    finish :: s -> a
    }

leftFold s p = LeftFold s p id

instance Functor (LeftFold x) where
    fmap f (LeftFold st1 p1 f2) = LeftFold st1 p1 (f . f2)

instance Applicative (LeftFold x) where
    pure x = LeftFold () const (const x)
    LeftFold st1 p1 f1 <*> LeftFold st2 p2 f2 = LeftFold {
        start   =                   st1 :!: st2,
        process = \(s1 :!: s2) x -> p1 s1 x :!: p2 s2 x,
        finish  = \(s1 :!: s2)   -> f1 s1 (f2 s2)
        }

runLeftFold :: LeftFold x a -> [x] -> a
runLeftFold (LeftFold st1 p1 f1) xs = f1 (foldl' p1 st1 xs)

monoidFold :: Monoid m => LeftFold m m
monoidFold = LeftFold mempty mappend id

mapElems :: LeftFold y a -> (x -> y) -> LeftFold x a 
mapElems (LeftFold s p f) t = LeftFold s (\s x -> p s (t x)) f

filterWith :: (x -> Bool) -> LeftFold (Bool :!: x) a -> LeftFold x a
filterWith p f = f `mapElems` (\x -> (p x :!: x))

onSelected :: LeftFold x a -> LeftFold (Bool :!: x) a
onSelected (LeftFold s p f) = LeftFold s (\s (b :!: x) -> if b then p s x else s) f

onJusts :: LeftFold x a -> LeftFold (Maybe x) a
onJusts (LeftFold s p f) = LeftFold s (\s mx -> maybe s (p s) mx) f

onAll :: LeftFold x a -> LeftFold (Bool :!: x) a
onAll lf = lf `mapElems` S.snd

runOnGroups :: (x -> x -> Bool) -> LeftFold x y -> LeftFold y z -> LeftFold x z
runOnGroups eq (LeftFold sti pi fi) (LeftFold sto po fo) = LeftFold (Nothing :!: sti :!: sto) go finish 
    where go (Nothing :!: si :!: so) x             = (Just x :!: pi si x  :!: so)
          go (Just x' :!: si :!: so) x | x `eq` x' = (Just x :!: pi si x  :!: so)
                                       | otherwise = (Just x :!: pi sti x :!: po so (fi si))
          finish (Nothing :!: si :!: so) = fo so
          finish (Just _  :!: si :!: so) = fo (po so (fi si))

runOnIntervals :: LeftFold x y -> LeftFold y z -> LeftFold (Bool :!: x) z
runOnIntervals (LeftFold si pi fi) (LeftFold so po fo) = LeftFold (Nothing :!: Nothing) go finish 
    where go (Just si :!: so) (True :!: x) = (Just (pi si x) :!: so)
          go (Just si :!: Just so) (False :!: x) = (Nothing :!: Just (po so (fi si)))
          go (Just si :!: Nothing) (False :!: x) = (Nothing :!: Just (po so (fi si)))
          go (Nothing :!: so) (True :!: x) = (Just (pi si x) :!: so)
          go (Nothing :!: so) (False :!: x) = (Nothing :!: so)
          finish (Nothing :!: Just so) = fo so
          finish (Nothing :!: Nothing) = fo so
          finish (Just si :!: Just so) = fo (po so (fi si))
          finish (Just si :!: Nothing) = fo (po so (fi si))

lfLength :: LeftFold x Int
lfLength = LeftFold 0 (\c _ -> c + 1) id

lfFirst :: LeftFold x (Maybe x)
lfFirst = getFirst <$> monoidFold `mapElems` (First . Just)

lfLast :: LeftFold x (Maybe x)
lfLast = getLast <$> monoidFold `mapElems` (Last . Just)

toList :: LeftFold x [x]
toList = LeftFold [] (flip (:)) reverse

concatFold :: LeftFold [x] [x]
concatFold = concat <$> toList


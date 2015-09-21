{-# LANGUAGE ExistentialQuantification, TypeOperators #-}

module LeftFold where

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Strict ((:!:), Pair((:!:)))
import qualified Data.Strict as S
import qualified Data.Map.Strict as M
import Data.Maybe


data LeftFold x a = forall s. LeftFold {
    start :: s,
    process :: s -> x -> s,
    finish :: s -> a
    }
    -- We keep things pure for as long as possible, to avoid constructing pairs
    -- in <*> when not needed. Some of the more advanced code below (e.g.
    -- intervals) is not properly tested with pure LeftFolds.
    | Pure a 

leftFold :: a -> (a -> x -> a) -> LeftFold x a
leftFold s p = LeftFold s p id

instance Functor (LeftFold x) where
    fmap f (Pure x) = Pure (f x)
    fmap f (LeftFold st1 p1 f2) = LeftFold st1 p1 (f . f2)

instance Applicative (LeftFold x) where
    pure x = Pure x
    Pure f <*> c = f <$> c
    LeftFold st1 p1 f1 <*> Pure x = LeftFold st1 p1 (\s -> f1 s x) 
    LeftFold st1 p1 f1 <*> LeftFold st2 p2 f2 = LeftFold {
        start   =                   st1 :!: st2,
        process = \(s1 :!: s2) x -> p1 s1 x :!: p2 s2 x,
        finish  = \(s1 :!: s2)   -> f1 s1 (f2 s2)
        }

runLeftFold :: LeftFold x a -> [x] -> a
runLeftFold (Pure x) _ = x
runLeftFold (LeftFold st1 p1 f1) xs = f1 $! foldl' p1 st1 xs

monoidFold :: Monoid m => LeftFold m m
monoidFold = leftFold mempty mappend

mapElems :: LeftFold y a -> (x -> y) -> LeftFold x a 
mapElems (Pure x) _ = (Pure x)
mapElems (LeftFold s p f) t = LeftFold s (\s x -> p s $! t x) f

filterElems :: (x -> Bool) -> LeftFold x a -> LeftFold x a 
filterElems _ (Pure x) = (Pure x)
filterElems pred (LeftFold s p f) = LeftFold s (\s x -> if pred x then p s x else s) f

adjoin :: (x -> Bool) -> LeftFold (Bool :!: x) a -> LeftFold x a
adjoin p f = f `mapElems` (\x -> (p x :!: x))


onSelected :: LeftFold x a -> LeftFold (Bool :!: x) a
onSelected (Pure x) = Pure x
onSelected (LeftFold s p f) = LeftFold s (\s (b :!: x) -> if b then p s x else s) f

onJusts :: LeftFold x a -> LeftFold (Maybe x) a
onJusts (Pure x) = Pure x
onJusts (LeftFold s p f) = LeftFold s (\s mx -> maybe s (p s) mx) f

onAll :: LeftFold x a -> LeftFold (Bool :!: x) a
onAll (Pure x) = Pure x
onAll lf = lf `mapElems` S.snd

runOnGroups :: (x -> x -> Bool) -> LeftFold x y -> LeftFold y z -> LeftFold x z
runOnGroups eq _ (Pure ox) = Pure ox
runOnGroups eq (Pure ix) (LeftFold sto po fo) = LeftFold (S.Nothing :!: sto) go finish 
    where go (S.Nothing :!: so) x             = (S.Just x :!: so)
          go (S.Just x' :!: so) x | x' `eq` x = (S.Just x :!: so)
                                  | otherwise = (S.Just x :!: po so ix)
          finish (S.Nothing :!: so) = fo so
          finish (S.Just _  :!: so) = fo (po so ix)
runOnGroups eq (LeftFold sti pi fi) (LeftFold sto po fo) = LeftFold (S.Nothing :!: sti :!: sto) go finish 
    where go (S.Nothing :!: si :!: so) x             = (S.Just x :!: pi si x  :!: so)
          go (S.Just x' :!: si :!: so) x | x' `eq` x = (S.Just x :!: pi si x  :!: so)
                                         | otherwise = (S.Just x :!: pi sti x :!: po so (fi si))
          finish (S.Nothing :!: si :!: so) = fo so
          finish (S.Just _  :!: si :!: so) = fo (po so (fi si))

runOnIntervals :: LeftFold x y -> LeftFold y z -> LeftFold (Bool :!: x) z
runOnIntervals _ (Pure ox) = (Pure ox)
runOnIntervals (Pure ix) (LeftFold so po fo) = LeftFold (False :!: S.Nothing) go finish 
    where go (True :!: so) (True :!: x)       = (True :!: so)
          go (True :!: S.Just so) (False :!: x) = (False :!: S.Just (po so ix))
          go (True :!: S.Nothing) (False :!: x) = (False :!: S.Just (po so ix))
          go (False :!: so) (True :!: x)      = (True :!: so)
          go (False :!: so) (False :!: x)     = (False :!: so)
          finish (False :!: S.Just so) = fo so
          finish (False :!: S.Nothing) = fo so
          finish (True  :!: S.Just so) = fo (po so ix)
          finish (True  :!: S.Nothing) = fo (po so ix)
runOnIntervals (LeftFold si pi fi) (LeftFold so po fo) = LeftFold (S.Nothing :!: S.Nothing) go finish 
    where go (S.Just si :!: so) (True :!: x) = (S.Just (pi si x) :!: so)
          go (S.Just si :!: S.Just so) (False :!: x) = (S.Nothing :!: S.Just (po so $! fi si))
          go (S.Just si :!: S.Nothing) (False :!: x) = (S.Nothing :!: S.Just (po so $! fi si))
          go (S.Nothing :!: so) (True :!: x) = (S.Just (pi si x) :!: so)
          go (S.Nothing :!: so) (False :!: x) = (S.Nothing :!: so)
          finish (S.Nothing :!: S.Just so) = fo so
          finish (S.Nothing :!: S.Nothing) = fo so
          finish (S.Just si :!: S.Just so) = fo (po so (fi si))
          finish (S.Just si :!: S.Nothing) = fo (po so (fi si))

multiplex :: Ord k => (a -> k) -> LeftFold a b -> LeftFold a (M.Map k b)
multiplex key (LeftFold si pi fi) = LeftFold M.empty go finish
    where go m x = M.alter go' (key x) m
            where go' mbOld = Just $ pi (fromMaybe si mbOld) x
          finish = M.map fi

lfLength :: LeftFold x Int
lfLength = leftFold 0 (\c _ -> c + 1)

lfFirst :: LeftFold x (Maybe x)
lfFirst = getFirst <$> monoidFold `mapElems` (First . Just)

lfLast :: LeftFold x (Maybe x)
lfLast = getLast <$> monoidFold `mapElems` (Last . Just)

toList :: LeftFold x [x]
toList = LeftFold [] (flip (:)) reverse

concatFold :: LeftFold [x] [x]
concatFold = concat <$> toList


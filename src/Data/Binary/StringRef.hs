{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances, OverlappingInstances#-}

module Data.Binary.StringRef 
        ( ListOfStringable(..)
        , StringReferencingBinary(..)
        , ls_encode
        , ls_decode
        ) where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Control.Monad
import Control.Applicative ((<$>))
import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.MyText as T
import Data.MyText (Text, decodeUtf8, encodeUtf8)
import Text.Printf
import Debug.Trace

class StringReferencingBinary a => ListOfStringable a where
  listOfStrings :: a -> [Text]

-- | An extended version of Binary that passes the list of strings of the
-- previous sample
class StringReferencingBinary a where
 ls_put :: [Text] -> a -> Put
 ls_get :: [Text] -> Get a

------------------------------------------------------------------------
-- Instances for the first few tuples

instance (StringReferencingBinary a, StringReferencingBinary b) => StringReferencingBinary (a,b) where
    ls_put strs (a,b)           = ls_put strs a >> ls_put strs b
    ls_get strs                 = liftM2 (,) (ls_get strs) (ls_get strs)

instance (StringReferencingBinary a, StringReferencingBinary b, StringReferencingBinary c) => StringReferencingBinary (a,b,c) where
    ls_put strs (a,b,c)         = ls_put strs a >> ls_put strs b >> ls_put strs c
    ls_get strs                 = liftM3 (,,) (ls_get strs) (ls_get strs) (ls_get strs)

instance (StringReferencingBinary a, StringReferencingBinary b, StringReferencingBinary c, StringReferencingBinary d) => StringReferencingBinary (a,b,c,d) where
    ls_put strs (a,b,c,d)       = ls_put strs a >> ls_put strs b >> ls_put strs c >> ls_put strs d
    ls_get strs                 = liftM4 (,,,) (ls_get strs) (ls_get strs) (ls_get strs) (ls_get strs)

instance (StringReferencingBinary a, StringReferencingBinary b, StringReferencingBinary c, StringReferencingBinary d, StringReferencingBinary e) => StringReferencingBinary (a,b,c,d,e) where
    ls_put strs (a,b,c,d,e)     = ls_put strs a >> ls_put strs b >> ls_put strs c >> ls_put strs d >> ls_put strs e
    ls_get strs                 = liftM5 (,,,,) (ls_get strs) (ls_get strs) (ls_get strs) (ls_get strs) (ls_get strs)


instance StringReferencingBinary a => StringReferencingBinary [a] where
    ls_put strs l  = ls_put strs (length l) >> mapM_ (ls_put strs) l
    ls_get strs    = do n <- (ls_get strs) :: Get Int
                        ls_getMany strs n

-- | 'ls_get strsMany n' ls_get strs 'n' elements in order, without blowing the stack.
ls_getMany :: StringReferencingBinary a => [Text] -> Int -> Get [a]
ls_getMany strs n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- ls_get strs
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE ls_getMany #-}


instance StringReferencingBinary Text where
        ls_put strs s = case elemIndex s strs of
                Just i | 0 <= i && i  < 255 - 2 ->
                        put (fromIntegral (succ i) :: Word8)
                _ ->    put (0 :: Word8) >> put s
        ls_get strs = do
                tag <- get
                case tag :: Word8 of
                  0 -> get
                  i -> let idx = fromIntegral (pred i) in
                       if idx < length strs
                       then return $! strs !! idx
                       else  fail $ printf "String reference %d out of range (%d strings available)" idx (length strs)


{-
instance Binary a => StringReferencingBinary a where
        ls_put _ = put
        ls_get _ = get
-}

instance StringReferencingBinary Char where { ls_put _ = put; ls_get _ = get }
instance StringReferencingBinary Int  where { ls_put _ = put; ls_get _ = get }
instance StringReferencingBinary Integer  where { ls_put _ = put; ls_get _ = get }
instance StringReferencingBinary Bool  where { ls_put _ = put; ls_get _ = get }

ls_encode :: StringReferencingBinary a => [Text] -> a -> ByteString
ls_encode strs = runPut . ls_put strs
{-# INLINE ls_encode #-}

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
--
ls_decode :: StringReferencingBinary a => [Text] -> ByteString -> a
ls_decode strs = runGet (ls_get strs)



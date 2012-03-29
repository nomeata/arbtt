{-# LANGUAGE OverloadedStrings #-}
module Data where

import Data.Time
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readPrec)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.StringRef
import qualified Data.MyText as T
import Data.MyText (Text)
import Control.Applicative
import Control.Monad
import Control.DeepSeq

type TimeLog a = [TimeLogEntry a]

data TimeLogEntry a = TimeLogEntry
        { tlTime :: UTCTime
        , tlRate :: Integer -- ^ in milli-seconds
        , tlData :: a }
  deriving (Show, Read)

instance Functor TimeLogEntry where
        fmap f tl = tl { tlData = f (tlData tl) }

instance NFData a => NFData (TimeLogEntry a) where
    rnf (TimeLogEntry a b c) = a `deepseq` b `deepseq` c `deepseq` ()

data CaptureData = CaptureData
        { cWindows :: [ (Bool, Text, Text) ]
                -- ^ Active window, window title, programm name
        , cLastActivity :: Integer -- ^ in milli-seconds
        }
  deriving (Show, Read)

instance NFData CaptureData where
    rnf (CaptureData a b) = a `deepseq` b `deepseq` ()

type ActivityData = [Activity]

data Activity = Activity 
        { activityCategory :: Maybe Category
        , activityName :: Text
        }
  deriving (Ord, Eq)

-- | An activity with special meaning: ignored by default (i.e. for idle times)
inactiveActivity = Activity Nothing "inactive"

instance Show Activity where
 show (Activity mbC t) = maybe "" ((++":").T.unpack) mbC ++ (T.unpack t)

instance Read Activity where
 readPrec = readP_to_Prec $ \_ ->
                   (do cat <- munch1 (/= ':')
                       char ':'
                       tag <- many1 ReadP.get
                       return $ Activity (Just (T.pack cat)) (T.pack tag))
                   <++ (Activity Nothing . T.pack <$> many1 ReadP.get)

type Category = Text

isCategory :: Category -> Activity -> Bool
isCategory cat (Activity (Just cat') _) = cat == cat'
isCategory _   _                        = False


-- Data.Binary instances

instance StringReferencingBinary a => StringReferencingBinary (TimeLogEntry a) where
 ls_put strs tle = do
        -- A version tag
        putWord8 1
        put (tlTime tle)
        put (tlRate tle)
        ls_put strs (tlData tle)
 ls_get strs = do
        v <- getWord8
        case v of
         1 -> TimeLogEntry <$> get <*> get <*> ls_get strs
         _ -> error $ "Unsupported TimeLogEntry version tag " ++ show v

instance Binary UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
        put d
        put (toRational t)
 get = do
        d <- get
        t <- get
        return $ UTCTime (ModifiedJulianDay d) (fromRational t)

instance ListOfStringable CaptureData where
  listOfStrings = concatMap (\(b,t,p) -> [t,p]) . cWindows

instance StringReferencingBinary CaptureData where
-- Versions:
-- 1 First version
-- 2 Using ListOfStringable
 ls_put strs cd = do
        -- A version tag
        putWord8 2
        ls_put strs (cWindows cd)
        ls_put strs (cLastActivity cd)
 ls_get strs = do
        v <- getWord8
        case v of
         1 -> CaptureData <$> get <*> get
         2 -> CaptureData <$> ls_get strs <*> ls_get strs
         _ -> error $ "Unsupported CaptureData version tag " ++ show v

  -- | 'getMany n' get 'n' elements in order, without blowing the stack.
  --   From Data.Binary
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

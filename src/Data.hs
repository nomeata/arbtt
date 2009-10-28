module Data where

import Data.Time
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readPrec)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Control.Applicative
import Control.Monad

type TimeLog a = [TimeLogEntry a]

data TimeLogEntry a = TimeLogEntry
	{ tlTime :: UTCTime
	, tlRate :: Integer -- ^ in milli-seconds
	, tlData :: a }
  deriving (Show)

instance Functor TimeLogEntry where
	fmap f tl = tl { tlData = f (tlData tl) }
	
data CaptureData = CaptureData
	{ cWindows :: [ (Bool, String, String) ]
		-- ^ Active window, window title, programm name
	, cLastActivity :: Integer -- ^ in milli-seconds
	}
  deriving (Show)

type ActivityData = [Activity]

data Activity = Activity 
	{ activityCategory :: Maybe Category
	, activityName :: String
	}
  deriving (Ord, Eq)

-- | An activity with special meaning: ignored by default (i.e. for idle times)
inactiveActivity = Activity Nothing "inactive"

instance Show Activity where
 show (Activity mbC t) = maybe "" (++":") mbC ++ t

instance Read Activity where
 readPrec = readP_to_Prec $ \_ ->
		   (do cat <- munch1 (/= ':')
		       char ':'
		       tag <- many1 ReadP.get
		       return $ Activity (Just cat) tag)
		   <++ (Activity Nothing `fmap` many1 ReadP.get)

type Category = String

isCategory :: Category -> Activity -> Bool
isCategory cat (Activity (Just cat') _) = cat == cat'
isCategory _   _                        = False


-- Data.Binary instances

instance Binary a => Binary (TimeLogEntry a) where
 put tle = do
 	-- A version tag
 	putWord8 1
	put (tlTime tle)
	put (tlRate tle)
	put (tlData tle)
 get = do
 	v <- getWord8
	when (v /= 1) $ error $ "Wrong TimeLogEntry version tag " ++ show v
	TimeLogEntry <$> get <*> get <*> get

instance Binary UTCTime where
 put (UTCTime (ModifiedJulianDay d) t) = do
 	put d
	put (toRational t)
 get = do
 	d <- get
	t <- get
	return $ UTCTime (ModifiedJulianDay d) (fromRational t)

instance Binary CaptureData where
 put cd = do
 	-- A version tag
 	putWord8 1
	put (cWindows cd)
	put (cLastActivity cd)
 get = do
 	v <- getWord8
	when (v /= 1) $ error $ "Wrong CaptureData version tag " ++ show v
	CaptureData <$> get <*> get

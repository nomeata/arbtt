module Data where

import Data.Time
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
import Text.ParserCombinators.ReadP
import Text.Read (readPrec)

type TimeLog a = [TimeLogEntry a]

data TimeLogEntry a = TimeLogEntry
	{ tlTime :: UTCTime
	, tlRate :: Integer -- ^ in milli-seconds
	, tlData :: a }
  deriving (Show, Read)

instance Functor TimeLogEntry where
	fmap f tl = tl { tlData = f (tlData tl) }
	
data CaptureData = CaptureData
	{ cWindows :: [ (Bool, String, String) ]
		-- ^ Active window, window title, programm name
	, cLastActivity :: Integer -- ^ in milli-seconds
	}
  deriving (Show, Read)

type ActivityData = [Activity]

data Activity = Activity 
	{ activityCategory :: Maybe Category
	, activityName :: String
	}
  deriving (Ord, Eq)

-- < An activity with special meaning: ignored by default (i.e. for idle times)
inactiveActivity = Activity Nothing "inactive"


instance Show Activity where
 show (Activity mbC t) = maybe "" (++":") mbC ++ t

instance Read Activity where
 readPrec = readP_to_Prec $ \_ ->
		   (do cat <- munch1 (/= ':')
		       char ':'
		       tag <- many1 get
		       return $ Activity (Just cat) tag)
		   <++ (Activity Nothing `fmap` many1 get)

type Category = String

isCategory :: Category -> Activity -> Bool
isCategory cat (Activity (Just cat') _) = cat == cat'
isCategory _   _                        = False


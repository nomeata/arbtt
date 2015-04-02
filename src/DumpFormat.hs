{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, CPP #-}
module DumpFormat
    ( DumpFormat(..)
    , readDumpFormat
    , dumpActivity
    , dumpSample
    , dumpSamples
    ) where

import Data.MyText (unpack, null, Text)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Time
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Char

import Data
import Text.Printf
import Data.List hiding (null)
import Prelude hiding (null)

data DumpFormat
    = DFShow
    | DFHuman
    | DFJSON 
    deriving (Show, Eq)

instance ToJSON Text where
    toJSON = toJSON . unpack

instance ToJSON (TimeLogEntry CaptureData) where
    toJSON (TimeLogEntry {..}) = object [
        "date" .= tlTime,
        "rate" .= tlRate,
        "inactive" .= cLastActivity tlData,
        "windows" .= map (\(a,p,t) -> object ["active" .= a, "program" .= p, "title" .= t]) (cWindows tlData),
        "desktop" .= cDesktop tlData
        ]

readDumpFormat :: String -> Maybe DumpFormat
readDumpFormat arg =
    case map toLower arg of
        "human"      -> return DFHuman
        "show"       -> return DFShow
        "json"       -> return DFJSON
        _            -> Nothing

dumpActivity :: TimeLog (CaptureData, TimeZone, ActivityData) -> IO ()
dumpActivity = mapM_ go
 where
    go tle = do
        dumpHeader tz (tlTime tle) (cLastActivity cd)
        dumpDesktop (cDesktop cd)
        mapM_ dumpWindow (cWindows cd)
        dumpTags ad
      where
        (cd, tz, ad) = tlData tle

dumpTags :: ActivityData -> IO ()
dumpTags = mapM_ go
  where go act = printf "    %s\n" (show act)

dumpHeader :: TimeZone -> UTCTime -> Integer -> IO ()
dumpHeader tz time lastActivity = do
    printf "%s (%dms inactive):\n"
        (formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz time))
        lastActivity

dumpWindow :: (Bool, Text, Text) -> IO ()
dumpWindow (active, title, program) = do
    printf "    %s %-15s %s\n"
        (if active then ("(*)"::String) else "( )")
        (unpack program ++ ":")
        (unpack title)

dumpDesktop :: Text -> IO ()
dumpDesktop d
    | null d    = return ()
    | otherwise = printf "    Current Desktop: %s\n" (unpack d)

dumpSample :: TimeZone -> TimeLogEntry CaptureData -> IO ()
dumpSample tz tle = do
    dumpHeader tz (tlTime tle) (cLastActivity (tlData tle))
    dumpDesktop (cDesktop (tlData tle))
    mapM_ dumpWindow (cWindows (tlData tle))

dumpSamples :: TimeZone -> DumpFormat -> TimeLog CaptureData -> IO ()
dumpSamples _ DFShow = mapM_ print

dumpSamples tz DFHuman = mapM_ (dumpSample tz)

dumpSamples _ DFJSON = enclose . sequence_ . intersperse (putStrLn ",") . map (LBS.putStr . encode)
  where
    enclose m = putStrLn "[" >> m >> putStrLn "]"

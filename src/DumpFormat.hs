{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
module DumpFormat where

import Data.MyText (unpack, Text)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Time
import System.Locale
import Data.Char

import Data
import Text.Printf
import Data.List

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
        "windows" .= map (\(a,p,t) -> object ["active" .= a, "program" .= p, "title" .= t]) (cWindows tlData)
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

dumpSamples :: TimeZone -> DumpFormat -> TimeLog CaptureData -> IO ()
dumpSamples _ DFShow = mapM_ print

dumpSamples tz DFHuman = mapM_ go
  where
    go tle = do
        dumpHeader tz (tlTime tle) (cLastActivity (tlData tle))
        mapM_ dumpWindow (cWindows (tlData tle))
dumpSamples _ DFJSON = enclose . sequence_ . intersperse (putStrLn ",") . map (LBS.putStr . encode)
  where
    enclose m = putStrLn "[" >> m >> putStrLn "]"

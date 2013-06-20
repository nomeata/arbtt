{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
module DumpFormat where

import Data.MyText (unpack, Text)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

import Data
import Text.Printf

data DumpFormat
    = DFShow
    | DFHuman
    | DFPrettyJSON
    | DFJSON 

instance ToJSON Text where
    toJSON = toJSON . unpack

instance ToJSON (TimeLogEntry CaptureData) where
    toJSON (TimeLogEntry {..}) = object [
        "date" .= tlTime,
        "rate" .= tlRate,
        "inactive" .= cLastActivity tlData,
        "windows" .= map (\(a,p,t) -> object ["active" .= a, "program" .= p, "title" .= t]) (cWindows tlData)
        ]

dumpSamples :: DumpFormat -> TimeLog CaptureData -> IO ()
dumpSamples DFShow = mapM_ print

dumpSamples DFHuman = mapM_ go
  where
    go tle = do 
        printf "%s (%dms inactive):\n" (show (tlTime tle)) (cLastActivity (tlData tle))
        mapM_ goW (cWindows (tlData tle))
    goW :: (Bool, Text, Text) -> IO ()
    goW (active, title, program) = do
        printf "    %s %-15s %s\n"
            (if active then ("(*)"::String) else "( )")
            (unpack program ++ ":")
            (unpack title)
             
dumpSamples DFJSON = LBS.putStr . encode

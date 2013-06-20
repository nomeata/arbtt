{-# LANGUAGE OverloadedStrings #-}
module DumpFormat where

import Data.MyText (unpack, Text)

import Data
import Text.Printf

data DumpFormat
    = DFShow
    | DFHuman
    | DFPrettyJSON
    | DFJSON 


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
             


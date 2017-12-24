{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import Control.Concurrent
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Exit
import System.Console.GetOpt
import System.Environment
import Data.Maybe
import Data.Version (showVersion)
import Data.Time.LocalTime

import Capture
import TimeLog
import UpgradeLog1
import CommonStartup
import DumpFormat
import LockFile

import Paths_arbtt (version)


data Options = Options
    { optSampleRate :: Integer
    , optLogFile :: String
    , optDump :: Bool
    }

defaultOptions :: FilePath -> Options
defaultOptions dir = Options
    { optSampleRate = 60
    , optLogFile = dir </> "capture.log"
    , optDump = False
    }

versionStr = "arbtt-capture " ++ showVersion version
header = "Usage: arbtt-capture [OPTIONS...]"

options :: [OptDescr (Options -> IO Options)]
options =
     [ Option "h?"     ["help"]
              (NoArg $ \_ -> do
                    putStr (usageInfo header options)
                    exitSuccess
              )
              "show this help"
     , Option "V"      ["version"]
              (NoArg $ \_ -> do
                    putStrLn versionStr
                    exitSuccess
              )
              "show the version number"
     , Option "f"      ["logfile"]
              (ReqArg (\arg opt -> return opt { optLogFile = arg }) "FILE")
               "use this file instead of ~/.arbtt/capture.log"
     , Option "r"      ["sample-rate"]
              (ReqArg (\arg opt -> return opt { optSampleRate = read arg }) "RATE")
              "set the sample rate in seconds (default: 60)"
     , Option "d"       ["dump"]
              (NoArg (\opt -> return opt { optDump = True }))
              "dump one sample to standard out, instead of modifying the log file"
     ]


main = do
    commonStartup
    
    args <- getArgs
    actions <- case getOpt Permute options args of
          (o,[],[])  -> return o
          (_,_,errs) -> do
                hPutStr stderr (concat errs ++ usageInfo header options)
                exitFailure

    dir <- getAppUserDataDirectory "arbtt"
    flags <- foldl (>>=) (return (defaultOptions dir)) actions

    if optDump flags then do
        setupCapture
        tz <- getCurrentTimeZone
        captureData >>= mkTimeLogEntry (optSampleRate flags * 1000) >>= dumpSample tz
      else do
        createDirectoryIfMissing False dir
        lockFile (optLogFile flags)
        upgradeLogFile1 (optLogFile flags)
        setupCapture
        runLogger (optLogFile flags) (optSampleRate flags * 1000) captureData

{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import Control.Concurrent
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
#ifdef WIN32
import System.Win32.Mutex
#else
import System.Posix.IO
#endif
import System.Exit
import System.Console.GetOpt
import System.Environment
import Data.Maybe
import Data.Version (showVersion)

import Capture
import TimeLog
import UpgradeLog1
import CommonStartup

import Paths_arbtt (version)


data Options = Options
    { optSampleRate :: Integer
    , optLogFile :: String
    }

defaultOptions :: FilePath -> Options
defaultOptions dir = Options
    { optSampleRate = 60
    , optLogFile = dir </> "capture.log"
    }

versionStr = "arbtt-capture " ++ showVersion version
header = "Usage: arbtt-capture [OPTIONS...]"

options :: [OptDescr (Options -> IO Options)]
options =
     [ Option "h?"     ["help"]
              (NoArg $ \_ -> do
                    hPutStr stderr (usageInfo header options)
                    exitSuccess
              )
              "show this help"
     , Option "V"      ["version"]
              (NoArg $ \_ -> do
                    hPutStrLn stderr versionStr
                    exitSuccess
              )
              "show the version number"
     , Option "f"      ["logfile"]
              (ReqArg (\arg opt -> return opt { optLogFile = arg }) "FILE")
               "use this file instead of ~/.arbtt/capture.log"
     , Option "r"      ["sample-rate"]
              (ReqArg (\arg opt -> return opt { optSampleRate = read arg }) "RATE")
              "set the sample rate in seconds (default: 60)"
     ]

-- | This is very raw, someone ought to improve this
lockFile filename = do
#ifdef WIN32
    success <- claimMutex filename
    unless success $ do
        hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for " ++ filename ++"!")
        exitFailure
#else
    flip catchIOError (\e -> hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for " ++ filename ++"!") >> exitFailure) $ do
        fd <- openFd (filename  ++ ".lck") WriteOnly (Just 0o644) defaultFileFlags
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
#endif       

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

    createDirectoryIfMissing False dir
    lockFile (optLogFile flags)
    upgradeLogFile1 (optLogFile flags)
    setupCapture
    runLogger (optLogFile flags) (optSampleRate flags * 1000) captureData

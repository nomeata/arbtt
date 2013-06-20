module Main where
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as M
import Data.Version (showVersion)
import Data.Maybe
import Control.Monad
import Data.Char

import TimeLog
import Data
import CommonStartup
import DumpFormat

import Paths_arbtt (version)

data Options = Options
    { optLogFile :: String
    , optFormat :: DumpFormat
    }

defaultOptions dir = Options
    { optLogFile = dir </> "capture.log"
    , optFormat = DFHuman
    }


versionStr = "arbtt-dump " ++ showVersion version
header = "Usage: arbtt-dump [OPTIONS...]"

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
     , Option "t"      ["format"]
              (ReqArg (\arg opt ->
                case map toLower arg of 
                    "human"      -> return $ opt { optFormat = DFHuman }
                    "show"       -> return $ opt { optFormat = DFShow }
                    "json"       -> return $ opt { optFormat = DFJSON }
                    _            -> do hPutStrLn stderr ("Invalid format \"" ++ arg ++ "\".")
                                       hPutStr stderr (usageInfo header options)
                                       exitFailure) "FORMAT")
               "output format, one of Human (default), Show or JSON "
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
  
  captures <- readTimeLog (optLogFile flags) :: IO (TimeLog CaptureData)
  dumpSamples (optFormat flags) captures

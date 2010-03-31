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
import Control.Applicative

import TimeLog
import Data
import CommonStartup

import Paths_arbtt (version)

data Options = Options
    { optLogFile :: String
    }

defaultOptions dir = Options
    { optLogFile = dir </> "capture.log"
    }

versionStr = "arbtt-import " ++ showVersion version
header = "Usage: arbtt-import [OPTIONS...]"

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
  
  ex <- doesFileExist (optLogFile flags)
  if ex
    then do
      putStrLn $ "File at " ++ (optLogFile flags) ++ " does already exist. Please delete this"
      putStrLn $ "file before running arbtt-import."
    else do
      captures <- map read . lines <$> getContents :: IO (TimeLog CaptureData)
      writeTimeLog (optLogFile flags) captures

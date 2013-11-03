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

import TimeLog
import Data
import CommonStartup

import Paths_arbtt (version)

data Options = Options
    { optInFile :: String
    , optOutFile :: String
    }

defaultOptions dir = Options
    { optInFile = dir </> "capture.log"
    , optOutFile = dir </> "capture.log.recovered"
    }


versionStr = "arbtt-recover " ++ showVersion version
header = "Usage: arbtt-recover [OPTIONS...]"

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
     , Option "i"      ["infile"]
              (ReqArg (\arg opt -> return opt { optInFile = arg }) "FILE")
              "read from this file instead of ~/.arbtt/capture.log"
     , Option "o"      ["outfile"]
              (ReqArg (\arg opt -> return opt { optOutFile = arg }) "FILE")
              "write to this file instead of ~/.arbtt/capture.log.recovered"
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

  captures <- recoverTimeLog (optInFile flags) :: IO (TimeLog CaptureData)
  writeTimeLog (optOutFile flags) captures
  putStrLn $ "Wrote data recovered from " ++ optInFile flags ++ " to " ++ optOutFile flags

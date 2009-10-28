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

import Paths_arbtt (version)

data Flag = Help | Version | LogFile String
        deriving Eq

versionStr = "arbtt-dump " ++ showVersion version
header = "Usage: arbtt-dump [OPTIONS...]"

options :: [OptDescr Flag]
options =
     [ Option "h?"     ["help"]
              (NoArg Help)
	      "show this help"
     , Option "V"      ["version"]
              (NoArg Version)
	      "show the version number"
     , Option "f"      ["logfile"]
     	       (ReqArg LogFile "FILE")
	       "use this file instead of ~/.arbtt/capture.log"
     ]


main = do
  args <- getArgs
  flags <- case getOpt Permute options args of
          (o,[],[]) | Help `notElem` o  && Version `notElem` o -> return o
          (o,_,_) | Version `elem` o -> do
                hPutStrLn stderr versionStr
                exitSuccess
          (o,_,_) | Help `elem` o -> do
                hPutStr stderr (usageInfo header options)
                exitSuccess
          (_,_,errs) -> do
                hPutStr stderr (concat errs ++ usageInfo header options)
                exitFailure

  dir <- getAppUserDataDirectory "arbtt"

  let captureFilename =
  	fromMaybe (dir </> "capture.log") $ listToMaybe $
 	mapMaybe (\f -> case f of { LogFile f -> Just f; _ -> Nothing}) $
	flags
  captures <- readTimeLog captureFilename :: IO (TimeLog CaptureData)
  mapM_ print captures

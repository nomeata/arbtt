module Main where
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as M
import Data.Version (showVersion)

import TimeLog
import Data

import Paths_arbtt (version)

data Flag = Help | Version
        deriving Eq

versionStr = "arbtt-dump " ++ showVersion version
header = "Usage: arbtt-dump [OPTIONS...]"

options :: [OptDescr Flag]
options =
     [ Option "h?"     ["help"]
              (NoArg Help)
	      "show this help"
     , Option ['V']     ["version"]
              (NoArg Version)
	      "show the version number"
     ]


main = do
  args <- getArgs
  case getOpt Permute options args of
          (o,[],[]) | Help `notElem` o  && Version `notElem` o -> return ()
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


  let captureFilename = dir </> "capture.log"
  captures <- readTimeLog captureFilename :: IO (TimeLog CaptureData)
  mapM_ print captures

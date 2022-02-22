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
import Data.List.TakeR
import Data.Time.LocalTime

import TimeLog
import Data
import CommonStartup
import DumpFormat

import Paths_arbtt (version)

data Options = Options
    { optLogFile :: String
    , optFormat :: DumpFormat
    , optFirst :: Maybe Int
    , optLast :: Maybe Int
    }

defaultOptions dir = Options
    { optLogFile = dir </> "capture.log"
    , optFormat = DFHuman
    , optFirst = Nothing
    , optLast = Nothing
    }


versionStr = "arbtt-dump " ++ showVersion version
header = "Usage: arbtt-dump [OPTIONS...]"

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
     , Option "t"      ["format"]
              (ReqArg (\arg opt ->
                case readDumpFormat arg of
                    Just fm -> return $ opt { optFormat = fm}
                    Nothing -> do
                        hPutStrLn stderr ("Invalid format \"" ++ arg ++ "\".")
                        hPutStr stderr (usageInfo header options)
                        exitFailure) "FORMAT")
               "output format, one of Human (default), Show or JSON "
     , Option "i"      ["first"]
              (ReqArg (\arg opt ->
                case reads arg of
                    [(n, "")] | n >= 0 -> return $ opt { optFirst = Just n }
                    _                  -> do
                        hPutStrLn stderr ("Invalid number \"" ++ arg ++ "\".")
                        hPutStr stderr (usageInfo header options)
                        exitFailure) "NUMBER")
               "only dump the first NUMBER of samples."
     , Option "l"      ["last"]
              (ReqArg (\arg opt ->
                case reads arg of
                    [(n, "")] | n >= 0 -> return $ opt { optLast = Just n }
                    _                  -> do
                        hPutStrLn stderr ("Invalid number \"" ++ arg ++ "\".")
                        hPutStr stderr (usageInfo header options)
                        exitFailure) "NUMBER")
               "only dump the last NUMBER of samples."
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

  captures <- case (optFirst flags, optLast flags) of
    (Nothing, Nothing) -> return captures
    (Just n , Nothing) -> return $ take n captures
    (Nothing, Just n ) -> return $ takeR n captures
    (Just _ , Just _ ) -> do
        hPutStrLn stderr "--first and --last are mutually exclusive"
        hPutStr stderr (usageInfo header options)
        exitFailure

  dumpSamples (optFormat flags) captures

{-# LANGUAGE CPP #-}
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
import Data.Conduit.Attoparsec
import Data.Conduit
import Data.Aeson (parseJSON, json)
import Data.Aeson.Types (parseEither)
import Data.Binary.StringRef
import Data.Attoparsec.ByteString.Char8 (skipSpace, option)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C

import TimeLog
import Data
import CommonStartup
import DumpFormat
import LockFile

import Paths_arbtt (version)

data Options = Options
    { optLogFile :: String
    , optFormat :: DumpFormat
    , optAppend :: Bool
    }

defaultOptions dir = Options
    { optLogFile = dir </> "capture.log"
    , optAppend = False
    , optFormat = DFShow
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
     , Option "a"      ["append"]
              (NoArg (\opt -> return opt { optAppend = True } ))
               "append to the logfile, instead of overriding it"
     , Option "t"      ["format"]
              (ReqArg (\arg opt ->
                case readDumpFormat arg of
                    Just DFHuman -> do
                        hPutStrLn stderr "The “Human” format cannot imported."
                        hPutStr stderr (usageInfo header options)
                        exitFailure
                    Just fm -> return $ opt { optFormat = fm}
                    Nothing -> do
                        hPutStrLn stderr ("Invalid format \"" ++ arg ++ "\".")
                        hPutStr stderr (usageInfo header options)
                        exitFailure
                    ) "FORMAT")
               "output format, one of Show (default) or JSON "
     ]

parseConduit :: DumpFormat -> Conduit BS.ByteString IO [TimeLogEntry CaptureData]
parseConduit DFHuman = error "Cannot read back human format"
parseConduit DFShow = C.lines =$= C.map ( (:[]) . read . BS.unpack)
parseConduit DFJSON =
        conduitParser (ignoreWhiteSpace json)
    =$= C.map snd
    =$= C.catMaybes
    =$= tlConduit
  where
    tlConduit = C.mapM $ \ v ->
        case parseEither oneOrMany v of
            Left e -> do
                hPutStrLn stderr ("Cannot parse log entry: " ++ e)
                exitFailure
            Right x -> pure x

    oneOrMany v = parseJSON v
            <|> ((:[]) <$> parseJSON v)

    ignoreWhiteSpace p = skipSpace *> option Nothing (Just <$> p)

binaryConduit :: ListOfStringable a =>
    Conduit [(TimeLogEntry a, Maybe a)] IO (Flush BS.ByteString)
binaryConduit = C.map (map go) =$= C.concatMap chunk
  where
    go (x,prev) = BSL.toStrict $ ls_encode strs x
        where strs = maybe [] listOfStrings prev
    chunk xs = map Chunk xs ++ [Flush]

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

  lockFile (optLogFile flags)

  ex <- doesFileExist (optLogFile flags)
  when (ex && not (optAppend flags)) $ do
      putStrLn $ "File at " ++ (optLogFile flags) ++ " does already exist. Please delete this"
      putStrLn $ "file before running arbtt-import."
      exitFailure
  createTimeLog False (optLogFile flags)

  h <- openBinaryFile (optLogFile flags) AppendMode
  runConduit $ C.sourceHandle stdin
    =$= parseConduit (optFormat flags)
    =$= stutterList
    =$= binaryConduit
    =$= C.sinkHandleFlush h
  hClose h


stutter :: Monad m => Conduit (TimeLogEntry a) m (TimeLogEntry a, Maybe a)
stutter =
    loop Nothing
  where
    loop prev = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just x -> do
                yield (x, prev)
                loop (Just (tlData x))

stutterList :: Monad m => Conduit [TimeLogEntry a] m [(TimeLogEntry a, Maybe a)]
stutterList =
    loop Nothing
  where
    loop prev = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just xs -> do
                let xs' = zip xs (prev : map (Just . tlData) xs)
                yield xs'
                loop (Just (tlData (last xs)))

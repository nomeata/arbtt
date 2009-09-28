module Main where

import Control.Monad
import Control.Concurrent

import Capture
import TimeLog
import System.Directory
import System.FilePath
import Graphics.X11.XScreenSaver (compiledWithXScreenSaver)
import System.IO
import System.Posix.IO
import System.IO.Error
import System.Exit
import System.Locale.SetLocale

-- | sampleRate in seconds
sampleRate = 60 

-- | This is very raw, someone ought to improve this
lockFile filename = flip catch (\e -> hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for " ++ filename ++"!") >> exitFailure) $ do
    fd <- openFd (filename  ++ ".lck") WriteOnly (Just 0644) defaultFileFlags
    setLock fd (WriteLock, AbsoluteSeek, 0, 0)

main = do
    setLocale LC_ALL (Just "") 
    unless compiledWithXScreenSaver $
    	hPutStrLn stderr "arbtt [Warning]: X11 was compiled without support for XScreenSaver"
    dir <- getAppUserDataDirectory "arbtt"
    createDirectoryIfMissing False dir
    let captureFile = dir </> "capture.log"
    lockFile captureFile
    runLogger captureFile (sampleRate * 1000) captureData

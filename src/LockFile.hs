{-# LANGUAGE CPP #-}
module LockFile where

import System.Exit
import System.IO
import System.IO.Error

#ifdef WIN32
import System.Win32.Mutex
#else
import System.Posix.IO
#endif

-- | This is very raw, someone ought to improve this
lockFile filename =
#ifdef WIN32
 do success <- claimMutex filename
    unless success $ do
        hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for " ++ filename ++"!")
        exitFailure
#else
    flip catchIOError (\e -> hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for " ++ filename ++"!") >> exitFailure) $ do
        fd <- openFd (filename  ++ ".lck") WriteOnly (Just 0o644) defaultFileFlags
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
#endif

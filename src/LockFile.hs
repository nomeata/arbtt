{-# LANGUAGE CPP #-}
module LockFile where

import System.Directory
import System.Exit
import System.IO
import System.IO.Error

#ifdef WIN32
import System.Win32.Mutex
import Control.Monad (unless)
#else
import System.Posix.IO
#endif

-- | This is very raw, someone ought to improve this
lockFile filename' = do
    filename <- canonicalizePath filename'
#ifdef WIN32
    success <- claimMutex filename
    unless success $ do
        hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for the path " ++ filename ++ ", possibly because another instance of arbtt is running.")
        exitFailure
#else
    flip catchIOError (\e -> hPutStrLn stderr ("arbtt [Error]: Could not aquire lock for " ++ filename ++"!") >> exitFailure) $ do
        let fileMode = Just 0o644
        fd <- openFd (filename  ++ ".lck") WriteOnly
#if MIN_VERSION_unix(2,8,0)
            (defaultFileFlags { creat = fileMode })
#else
            fileMode defaultFileFlags
#endif
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
#endif

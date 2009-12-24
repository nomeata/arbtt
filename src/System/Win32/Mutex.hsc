{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.Mutex 
	( claimMutex
	) where

import System.Win32.Types
import Foreign

#include <windows.h>

type LPSECURITY_ATTRIBUTES = Ptr ()

foreign import stdcall unsafe "windows.h CreateMutexW"
  c_CreateMutex :: LPSECURITY_ATTRIBUTES -> Bool -> LPCTSTR -> IO HANDLE

foreign import stdcall unsafe "windows.h SetLastError"
  c_SetLastError :: DWORD -> IO ()

-- | Given a unique identifier in the string, this function tries to create
--   and claim a mutex based on that name. Returns 'True' if the mutex was claimed.
claimMutex :: String -> IO Bool
claimMutex lockfilename = do
	let name = "Local\\" ++ filter (/='\\') lockfilename
	withTString name $ \c_str -> do
	c_SetLastError (#const ERROR_SUCCESS)
	handle <- c_CreateMutex nullPtr True c_str
	err <- getLastError
	if handle == nullPtr
	  then errorWin "CreateMutex"
	  else return (err /= (#const ERROR_ALREADY_EXISTS))


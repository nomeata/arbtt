{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Win32.Window.Extra
	( fetchWindowTitles
	, getForegroundWindow
	, getIdleTime
	) where

import Graphics.Win32.GDI.Types
import System.Win32.Types

import Foreign

import Data.IORef 
import Control.Monad (unless)

#include <windows.h>

type EnumWindowsProc = HWND -> LPARAM -> IO Bool

foreign import ccall "wrapper"
  mkEnumWindowsProc :: EnumWindowsProc -> IO (FunPtr EnumWindowsProc)

foreign import stdcall safe "windows.h EnumWindows"
  c_EnumWindows :: (FunPtr EnumWindowsProc) -> LPARAM -> IO Bool

foreign import stdcall unsafe "windows.h GetWindowTextW"
  c_GetWindowText :: HWND -> LPWSTR -> Int -> IO Int

foreign import stdcall unsafe "windows.h GetWindowTextLengthW"
  c_GetWindowTextLength :: HWND -> IO Int

foreign import stdcall unsafe "windows.h GetForegroundWindow"
  c_GetForegroundWindow :: IO HWND

foreign import stdcall unsafe "windows.h GetLastInputInfo"
  c_GetLastInputInfo :: Ptr LASTINPUTINFO -> IO Bool

foreign import stdcall unsafe "windows.h GetTickCount"
  c_GetTickCount :: IO DWORD

data LASTINPUTINFO = LASTINPUTINFO DWORD deriving (Show)

instance Storable LASTINPUTINFO where
    sizeOf = const (#size LASTINPUTINFO)
    alignment = sizeOf
    poke buf (LASTINPUTINFO t) = do
        (#poke LASTINPUTINFO, cbSize) buf ((#size LASTINPUTINFO) :: UINT)
        (#poke LASTINPUTINFO, dwTime) buf t
    peek buf = do
        t <- (#peek LASTINPUTINFO, dwTime) buf
        return $ LASTINPUTINFO t

fetchWindowTitles :: IO [(HWND, String)]
fetchWindowTitles = do
	resultRef <- newIORef []
	callback <- mkEnumWindowsProc $ \x y -> do
		len <- c_GetWindowTextLength x
		allocaArray0 len  $ \c_test -> do
			r <- c_GetWindowText x c_test (len+1)
			err <- getLastError
			if err /= (#const ERROR_SUCCESS)
			 then errorWin "GetWindowText"
			 else do
			str <- peekTString c_test
			unless (null str) $
				modifyIORef resultRef ((x,str):)
			return True
	c_EnumWindows callback 0
	readIORef resultRef

getForegroundWindow :: IO HWND
getForegroundWindow = c_GetForegroundWindow

-- | Idle time in milliseconds
getIdleTime :: IO Integer
getIdleTime = with (LASTINPUTINFO 0) $ \lii_p -> do
		failIfFalse_ "GetLastInputInfo" $ c_GetLastInputInfo lii_p
		LASTINPUTINFO lii <- peek lii_p
		now <- c_GetTickCount
		return (fromIntegral (now - lii))

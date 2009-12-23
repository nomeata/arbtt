{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Win32.Window.Extra
	( fetchWindowTitles
	, getForegroundWindow
	, getIdleTime
	) where

import Graphics.Win32.GDI.Types
import System.Win32.Types
import System.Win32.Process
import System.Win32.File    ( closeHandle )
import System.IO
import Control.Exception    ( bracket )
import Control.Monad

import Foreign

import Data.IORef 

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

foreign import stdcall unsafe "windows.h GetWindowThreadProcessId"
  c_GetWindowThreadProcessId :: HWND -> LPDWORD -> IO DWORD

foreign import stdcall unsafe "psapi.h GetProcessImageFileNameW"
  c_GetProcessImageFileName :: HANDLE -> LPTSTR -> DWORD -> IO DWORD

foreign import stdcall unsafe "windows.h SetLastError"
  c_SetLastError :: DWORD -> IO ()

foreign import stdcall unsafe "windows.h IsWindowVisible"
  c_IsWindowVisible :: HWND -> IO Bool

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

fetchWindowTitles :: IO [(HWND, String,String)]
fetchWindowTitles = do
	resultRef <- newIORef []
	callback <- mkEnumWindowsProc $ \winh _ -> do
                v <- c_IsWindowVisible winh -- only consider visible windows
                if not v then return True else do
                proc <- alloca $ \pid_p -> do
                        c_GetWindowThreadProcessId winh pid_p 
                        pid <- peek pid_p
                        bracket (openProcess pROCESS_QUERY_INFORMATION False pid) closeHandle $ \ph ->
                                allocaArray0 (#const MAX_PATH) $ \c_test -> do
                                        c_SetLastError (#const ERROR_SUCCESS)
                                        r <- c_GetProcessImageFileName ph c_test ((#const MAX_PATH)+1)
                                        err <- getLastError
                                        if r == 0 && err /= (#const ERROR_SUCCESS)
                                         then do hPutStrLn stderr $ "GetProcessImageFileName returned error " ++ show err ++ "."
                                                 return ""
                                         else peekTString c_test
                                         
		len <- c_GetWindowTextLength winh
		str <- allocaArray0 len  $ \c_test -> do
                        c_SetLastError (#const ERROR_SUCCESS)
			r <- c_GetWindowText winh c_test (len+1)
			err <- getLastError
			if r == 0 && err /= (#const ERROR_SUCCESS)
                         then do hPutStrLn stderr $ "GetWindowText returned error " ++ show err ++ "."
                                 return ""
			 else peekTString c_test
                unless (str `elem` ["", "Default IME"]) $ do -- Ignore some windows by default
                        modifyIORef resultRef ((winh,str,proc):)
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

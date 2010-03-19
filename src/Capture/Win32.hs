module Capture.Win32 where

import Data
import Control.Monad
import Control.Exception (bracket)
import Control.Applicative
import Data.Maybe
import Data.Time.Clock
import System.IO

import Graphics.Win32.Window.Extra

setupCapture :: IO ()
setupCapture = do
        return ()

captureData :: IO CaptureData
captureData = do
        titles <- fetchWindowTitles
        foreground <- getForegroundWindow

        let winData = map (
                \(h,t,p) -> (h == foreground, t, p)
                ) titles

        it <- fromIntegral `fmap` getIdleTime

        return $ CaptureData winData it

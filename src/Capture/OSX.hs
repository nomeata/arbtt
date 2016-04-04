module Capture.OSX where

import Data
import qualified Data.MyText as T
import Control.Monad
import Control.Applicative

import Graphics.OSX.Window

setupCapture :: IO ()
setupCapture = do
        return ()

captureData :: IO CaptureData
captureData = do
        titles <- fetchWindowTitles
        foreground <- getForegroundWindow

        let winData = map (
                \(h,t,p) -> (h == foreground, T.pack t, T.pack p)
                ) titles

        it <- fromIntegral `fmap` getIdleTime

        return $ CaptureData winData it (T.pack "")

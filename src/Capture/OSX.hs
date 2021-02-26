module Capture.OSX where

import Data
import Data.Default.Class
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

        let winData = [ def{ wActive = h == foreground
                           , wTitle = T.pack t
                           , wProgram = T.pack p
                           }
                      | (h, t, p) <- titles ]

        it <- fromIntegral `fmap` getIdleTime

        return $ CaptureData winData it (T.pack "")

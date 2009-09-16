module Capture where

import Data
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.XScreenSaver (getXIdleTime)
import Control.Monad
import Data.Maybe
import Control.Applicative
import Data.Time.Clock

captureData :: IO CaptureData
captureData = do
	dpy <- openDisplay ":0"
        xSetErrorHandler
	let rwin = defaultRootWindow dpy

	a <- internAtom dpy "_NET_CLIENT_LIST" False
	p <- getWindowProperty32 dpy a rwin
	let cwins = maybe [] (map fromIntegral) p

	(fwin,_) <- getInputFocus dpy
	winData <- mapM (\w -> (,,) (w == fwin) <$> getWindowTitle dpy w <*> getProgramName dpy w) cwins

	it <- fromIntegral `fmap` getXIdleTime dpy

	closeDisplay dpy
	return $ CaptureData winData it

getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy w = fmap (fromMaybe "") $ fetchName dpy w

getProgramName :: Display -> Window -> IO String
getProgramName dpy w = fmap resName $ getClassHint dpy w


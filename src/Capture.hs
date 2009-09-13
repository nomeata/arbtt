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
	(_,_,cwins) <- queryTree dpy rwin

	cwins <- filterM (isInterestingWindow dpy) cwins
	(fwin,_) <- getInputFocus dpy
	winData <- mapM (\w -> (,,) (w == fwin) <$> getWindowTitle dpy w <*> getProgramName dpy w) cwins

	it <- fromIntegral `fmap` getXIdleTime dpy

	closeDisplay dpy
	return $ CaptureData winData it

-- copied from XMonad/Main.hs, where function "ok" from "scan"
isInterestingWindow :: Display -> Window -> IO Bool
isInterestingWindow dpy w = do
	wa <- getWindowAttributes dpy w
	a  <- internAtom dpy "WM_STATE" False
	p  <- getWindowProperty32 dpy a w
	let ic = case p of
		  Just (3:_) -> True -- 3 for iconified
		  _          -> False
	return $ not (wa_override_redirect wa)
		 && (wa_map_state wa == waIsViewable || ic)


getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy w = fmap (fromMaybe "") $ fetchName dpy w

getProgramName :: Display -> Window -> IO String
getProgramName dpy w = fmap resName $ getClassHint dpy w


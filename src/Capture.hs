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

	cwins <- case p of
		Just wins -> return (map fromIntegral wins)
		Nothing -> do (_,_,cwins) <- queryTree dpy rwin
		              filterM (isInterestingWindow dpy) cwins

	(fsubwin,_) <- getInputFocus dpy
	fwin <- followTreeUntil dpy (`elem` cwins) fsubwin

	winData <- mapM (\w -> (,,) (w == fwin) <$> getWindowTitle dpy w <*> getProgramName dpy w) cwins

	it <- fromIntegral `fmap` getXIdleTime dpy

	closeDisplay dpy
	return $ CaptureData winData it

getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy = fmap (fromMaybe "") . fetchName dpy

getProgramName :: Display -> Window -> IO String
getProgramName dpy = fmap resName . getClassHint dpy

-- | Follows the tree of windows up until the condition is met or the root
-- window is reached.
followTreeUntil :: Display -> (Window -> Bool) -> Window -> IO Window 
followTreeUntil dpy cond = go
  where go w | cond w    = return w
             | otherwise = do (r,p,_) <- queryTree dpy w
	                      if p == 0 then return w
			                else go p 

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


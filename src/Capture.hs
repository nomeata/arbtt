module Capture where

import Data
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.XScreenSaver (getXIdleTime)
import Control.Monad
import Data.Maybe
import Control.Applicative
import Data.Time.Clock
import System.IO

captureData :: IO CaptureData
captureData = do
	dpy <- openDisplay ""
        xSetErrorHandler
	let rwin = defaultRootWindow dpy

	a <- internAtom dpy "_NET_CLIENT_LIST" False
	p <- getWindowProperty32 dpy a rwin

	wins <- case p of
		Just wins -> return (map fromIntegral wins)
		Nothing -> do hPutStrLn stderr "arbtt: ERROR: No _NET_CLIENT_LIST set for the root window"
		              return []

	(fsubwin,_) <- getInputFocus dpy
	fwin <- followTreeUntil dpy (`elem` wins) fsubwin

	winData <- mapM (\w -> (,,) (w == fwin) <$> getWindowTitle dpy w <*> getProgramName dpy w) wins

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

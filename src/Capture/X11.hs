module Capture.X11 where

import Data
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Control.Monad
import Control.Exception (bracket)
import Control.Applicative
import Data.Maybe
import Data.Time.Clock
import System.IO

import System.Locale.SetLocale
import Graphics.X11.XScreenSaver (getXIdleTime, compiledWithXScreenSaver)

setupCapture :: IO ()
setupCapture = do
        unless compiledWithXScreenSaver $
                hPutStrLn stderr "arbtt [Warning]: X11 was compiled without support for XScreenSaver"
        dpy <- openDisplay ""
        xSetErrorHandler
        let rwin = defaultRootWindow dpy
        a <- internAtom dpy "_NET_CLIENT_LIST" False
        p <- getWindowProperty32 dpy a rwin
        when (isNothing p) $ do
                hPutStrLn stderr "arbtt: ERROR: No _NET_CLIENT_LIST set for the root window"
        closeDisplay dpy

captureData :: IO CaptureData
captureData = do
        dpy <- openDisplay ""
        xSetErrorHandler
        let rwin = defaultRootWindow dpy

        a <- internAtom dpy "_NET_CLIENT_LIST" False
        p <- getWindowProperty32 dpy a rwin

        wins <- case p of
                Just wins -> return (map fromIntegral wins)
                Nothing   -> return []

        (fsubwin,_) <- getInputFocus dpy
        fwin <- followTreeUntil dpy (`elem` wins) fsubwin

        winData <- mapM (\w -> (,,) (w == fwin) <$> getWindowTitle dpy w <*> getProgramName dpy w) wins

        it <- fromIntegral `fmap` getXIdleTime dpy

        closeDisplay dpy
        return $ CaptureData winData it

getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy =  myFetchName dpy

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

-- | better than fetchName from X11, as it supports _NET_WM_NAME and unicode
--
-- Code taken from XMonad.Managehook.title
myFetchName :: Display -> Window -> IO String
myFetchName d w = do
        let getProp =
                (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `catch`
                (\_ -> getTextProperty d w wM_NAME)

            extract prop = do l <- wcTextPropertyToTextList d prop
                              return $ if null l then "" else head l

        bracket getProp (xFree . tp_value) extract `catch` \_ -> return ""


module Capture.X11 where

import Data
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Control.Monad
import Control.Exception (bracket)
import System.IO.Error (catchIOError)
import Control.Applicative
import Data.Maybe
import Data.Time.Clock
import Data.Default.Class
import System.IO
import qualified Data.MyText as T

import System.Locale.SetLocale
import Graphics.X11.XScreenSaver (getXIdleTime, compiledWithXScreenSaver)

setupCapture :: IO ()
setupCapture = do
        loc <- supportsLocale
        unless loc $ hPutStrLn stderr "arbtt [Warning]: locale unsupported"
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

        -- Desktops
        current_desktop <- flip catchIOError (\_ -> return "") $ do
            a <- internAtom dpy "_NET_CURRENT_DESKTOP" False
            p <- getWindowProperty32 dpy a rwin
            let desk_index = do {[d] <- p; return (fromIntegral d)}

            a <- internAtom dpy "_NET_DESKTOP_NAMES" False
            tp <- getTextProperty dpy rwin a
            names <- wcTextPropertyToTextList dpy tp

            return $ case desk_index of
                  Nothing -> ""
                  Just n -> if 0 <= n && n < length names
                            then names !! n
                            else show n
        -- Windows
        a <- internAtom dpy "_NET_CLIENT_LIST" False
        p <- getWindowProperty32 dpy a rwin

        wins <- case p of
                Just wins -> filterM (isInteresting dpy) (map fromIntegral wins)
                Nothing   -> return []

        (fsubwin,_) <- getInputFocus dpy
        fwin <- followTreeUntil dpy (`elem` wins) fsubwin

        winData <- forM wins $ \w -> do
            title <- T.pack <$> getWindowTitle dpy w
            program <- T.pack <$> getProgramName dpy w
            return def{ wActive = w == fwin, wTitle = title, wProgram = program }

        it <- fromIntegral `fmap` getXIdleTime dpy

        closeDisplay dpy
        return $ CaptureData winData it (T.pack current_desktop)

getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy =  myFetchName dpy

getProgramName :: Display -> Window -> IO String
getProgramName dpy = fmap resName . getClassHint dpy

-- Returns a parent of a window or zero.
getParent :: Display -> Window -> IO Window
getParent dpy w = do
  (_, parent, _) <- queryTree dpy w `catchIOError` (const $ return (0,0,[]))
  return parent

-- | Follows the tree of windows up until the condition is met or the root
-- window is reached.
followTreeUntil :: Display -> (Window -> Bool) -> Window -> IO Window 
followTreeUntil dpy cond = go
  where go w | cond w    = return w
             | otherwise = do p <- getParent dpy w
                              if p == 0 then return w
                                        else go p 

-- | Ignore, for example, Desktop and Docks windows
isInteresting :: Display -> Window -> IO Bool
isInteresting d w = do
    a <- internAtom d "_NET_WM_WINDOW_TYPE" False
    dock <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
    desk <- internAtom d "_NET_WM_WINDOW_TYPE_DESKTOP" False
    mbr <- getWindowProperty32 d a w
    case mbr of
        Just [r] -> return $ fromIntegral r `notElem` [dock, desk]
        _        -> return True

-- | better than fetchName from X11, as it supports _NET_WM_NAME and unicode
--
-- Code taken from XMonad.Managehook.title
myFetchName :: Display -> Window -> IO String
myFetchName d w = do
        let getProp =
                (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `catchIOError`
                (\_ -> getTextProperty d w wM_NAME)

            extract prop = do l <- wcTextPropertyToTextList d prop
                              return $ if null l then "" else head l

        bracket getProp (xFree . tp_value) extract
            `catchIOError` \_ -> return ""


{-# LANGUAGE  ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    : Graphics.X11.XScreenSaver
-- Copyright : (c) Joachim Breitner
-- License   : GPL2
--
-- Maintainer: Joachim Breitner <mail@joachim-breitner.de>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
--
-- Interface to XScreenSaver API
--

module Graphics.X11.XScreenSaver (
    getXIdleTime,
    XScreenSaverState(..),
    XScreenSaverKind(..),
    XScreenSaverInfo(..),
    xScreenSaverQueryExtension,
    xScreenSaverQueryVersion,
    xScreenSaverQueryInfo,
    compiledWithXScreenSaver
 ) where

import Foreign
import Foreign.C.Types
import Graphics.X11.Xlib
import Control.Monad

data XScreenSaverState = ScreenSaverOff | ScreenSaverOn | ScreenSaverDisabled deriving Show
data XScreenSaverKind = ScreenSaverBlanked | ScreenSaverInternal | ScreenSaverExternal deriving Show

-- | Representation of the XScreenSaverInfo struct.
data XScreenSaverInfo = XScreenSaverInfo
                          { xssi_window        :: !Window,
                            xssi_state         :: !XScreenSaverState,
-- ^ The state field specified whether or not the screen saver is currently
-- active and how the til-or-since value should be interpreted:
--
-- ['ScreenSaverOff'] The  screen is not currently being saved; til-or-since specifies the
-- number of milliseconds until the screen saver is expected to activate.
--
-- ['ScreenSaverOn'] The screen is currently being saved; til-or-since specifies the number
-- of milliseconds since the screen saver activated.
--
-- ['ScreenSaverDisabled'] The screen saver is currently disabled; til-or-since is zero.
                            xssi_kind          :: !XScreenSaverKind,
-- ^ The kind field specifies the mechanism that either is currently being used
-- or would have been were the screen being saved:
--
-- ['ScreenSaverBlanked'] The video signal to the display monitor was disabled.
--
-- ['ScreenSaverInternal'] A server-dependent, built-in screen saver image was displayed;
-- either no client had set the screen saver window attributes or a different
-- client had the server grabbed when the screen saver activated.  
--
-- ['ScreenSaverExternal'] The screen saver window was mapped with attributes set by a client
-- using the ScreenSaverSetAttributes request.  
                            xssi_til_or_since  :: !CULong,
                            xssi_idle          :: !CULong,
-- ^ The idle field specifies the number of milliseconds since the last input
-- was received from the user on any of the input devices.
                            xssi_event_mask    :: !CULong
-- ^ The event-mask field specifies which, if any, screen saver events this
-- client has requested using ScreenSaverSelectInput.
                            } deriving (Show)

-- | Simple wrapper around 'xScreenSaverQueryInfo' if you are only interested in
-- the idle time, in milliseconds. Returns 0 if the XScreenSaver extension is
-- not available
getXIdleTime :: Display -> IO Int
getXIdleTime dpy = maybe 0 (fromIntegral . xssi_idle) `fmap` xScreenSaverQueryInfo dpy

-- We have XScreenSaver, so the library will actually work
compiledWithXScreenSaver :: Bool
compiledWithXScreenSaver = True

-- for XFree() (already included from scrnsaver.h, but I don't know if I can count on that.)
#include <X11/Xlib.h>
#include <X11/extensions/scrnsaver.h>

xScreenSaverState2CInt :: XScreenSaverState -> CInt
xScreenSaverState2CInt ScreenSaverOn = #const ScreenSaverOn
xScreenSaverState2CInt ScreenSaverOff = #const ScreenSaverOff
xScreenSaverState2CInt ScreenSaverDisabled = #const ScreenSaverDisabled

cInt2XScreenSaverState :: CInt -> XScreenSaverState
cInt2XScreenSaverState (#const ScreenSaverOn) = ScreenSaverOn
cInt2XScreenSaverState (#const ScreenSaverOff) = ScreenSaverOff
cInt2XScreenSaverState (#const ScreenSaverDisabled) = ScreenSaverDisabled
cInt2XScreenSaverState _ = error "Unknown state in xScreenSaverQueryInfo"

instance Storable XScreenSaverState where
  sizeOf    _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  poke p xsss = poke (castPtr p) (xScreenSaverState2CInt xsss)
  peek p = cInt2XScreenSaverState `fmap` peek (castPtr p)
 

xScreenSaverKind2CInt :: XScreenSaverKind -> CInt
xScreenSaverKind2CInt ScreenSaverBlanked = #const ScreenSaverBlanked
xScreenSaverKind2CInt ScreenSaverInternal = #const ScreenSaverInternal
xScreenSaverKind2CInt ScreenSaverExternal = #const ScreenSaverExternal

cInt2XScreenSaverKind :: CInt -> XScreenSaverKind
cInt2XScreenSaverKind (#const ScreenSaverBlanked) = ScreenSaverBlanked
cInt2XScreenSaverKind (#const ScreenSaverInternal) = ScreenSaverInternal
cInt2XScreenSaverKind (#const ScreenSaverExternal) = ScreenSaverExternal
cInt2XScreenSaverKind _ = error "Unknown kind in xScreenSaverQueryInfo"

instance Storable XScreenSaverKind where
  sizeOf    _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  poke p xsss = poke (castPtr p) (xScreenSaverKind2CInt xsss)
  peek p = cInt2XScreenSaverKind `fmap` peek (castPtr p)
 

instance Storable XScreenSaverInfo where
  sizeOf _ = #{size XScreenSaverInfo}
  -- FIXME: Is this right?
  alignment _ = alignment (undefined :: CInt)

  poke p xssi = do
    #{poke XScreenSaverInfo, window       } p $ xssi_window xssi
    #{poke XScreenSaverInfo, state        } p $ xssi_state xssi
    #{poke XScreenSaverInfo, kind         } p $ xssi_kind xssi
    #{poke XScreenSaverInfo, til_or_since } p $ xssi_til_or_since xssi
    #{poke XScreenSaverInfo, idle         } p $ xssi_idle xssi
    #{poke XScreenSaverInfo, eventMask    } p $ xssi_event_mask xssi

  peek p = return XScreenSaverInfo
              `ap` (#{peek XScreenSaverInfo, window} p)
              `ap` (#{peek XScreenSaverInfo, state} p)
              `ap` (#{peek XScreenSaverInfo, kind} p)
              `ap` (#{peek XScreenSaverInfo, til_or_since} p)
              `ap` (#{peek XScreenSaverInfo, idle} p)
              `ap` (#{peek XScreenSaverInfo, eventMask} p)


xScreenSaverQueryExtension :: Display -> IO (Maybe (CInt, CInt))
xScreenSaverQueryExtension dpy = wrapPtr2 (cXScreenSaverQueryExtension dpy) go
  where go False _ _                = Nothing
        go True eventbase errorbase = Just (fromIntegral eventbase, fromIntegral errorbase)

xScreenSaverQueryVersion :: Display -> IO (Maybe (CInt, CInt))
xScreenSaverQueryVersion dpy = wrapPtr2 (cXScreenSaverQueryVersion dpy) go
  where go False _ _        = Nothing
        go True major minor = Just (fromIntegral major, fromIntegral minor)

wrapPtr2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> (c -> a -> b -> d) -> IO d
wrapPtr2 cfun f =
  withPool $ \pool -> do aptr <- pooledMalloc pool
                         bptr <- pooledMalloc pool
                         ret <- cfun aptr bptr
                         a <- peek aptr
                         b <- peek bptr
                         return (f ret a b)

-- | xScreenSaverQueryInfo returns information about the current state of the
-- screen server. If the xScreenSaver extension is not available, it returns Nothing
xScreenSaverQueryInfo :: Display -> IO (Maybe XScreenSaverInfo)
xScreenSaverQueryInfo dpy = do
  p <- cXScreenSaverAllocInfo
  if p == nullPtr then return Nothing else do
  s <- cXScreenSaverQueryInfo dpy (defaultRootWindow dpy) p
  if s == 0 then return Nothing else do
  xssi <- peek p
  cXFree p
  return (Just xssi)

foreign import ccall "XScreenSaverQueryExtension"
  cXScreenSaverQueryExtension :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall "XScreenSaverQueryVersion"
  cXScreenSaverQueryVersion :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall "XScreenSaverAllocInfo"
 cXScreenSaverAllocInfo :: IO (Ptr XScreenSaverInfo)

foreign import ccall "XScreenSaverQueryInfo"
 cXScreenSaverQueryInfo :: Display -> Drawable -> Ptr XScreenSaverInfo -> IO Status

foreign import ccall "XFree"
  cXFree :: Ptr a -> IO CInt

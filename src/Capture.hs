{-# LANGUAGE CPP #-}

module Capture (
#if defined(WIN32)
        module Capture.Win32
#elif defined(DARWIN)
        module Capture.OSX
#else
        module Capture.X11
#endif
        ) where

#if defined(WIN32)
import Capture.Win32
#elif defined(DARWIN)
import Capture.OSX
#else
import Capture.X11
#endif

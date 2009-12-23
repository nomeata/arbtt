{-# LANGUAGE CPP #-}

module Capture (
#ifdef WIN32
	module Capture.Win32
#else
	module Capture.X11
#endif
	) where

#ifdef WIN32
import Capture.Win32
#else
import Capture.X11
#endif

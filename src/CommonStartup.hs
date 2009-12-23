{-# LANGUAGE CPP #-}

module CommonStartup where

#ifndef WIN32
import System.Locale.SetLocale
#endif

commonStartup :: IO ()
commonStartup = do
#ifndef WIN32
	setLocale LC_ALL (Just "") 
#endif
	return ()

module TimeLog where

import Data

import Control.Applicative
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Time
import Data.Binary
import Data.Binary.Get
import Data.Function
import Data.Char
import System.Directory

import qualified Data.ByteString.Lazy as BS

magic = BS.pack $ map (fromIntegral.ord) $ "arbtt-timelog-v1\n"

-- | Runs the given action each delay milliseconds and appends the TimeLog to the
-- given file.
runLogger :: Binary a => FilePath -> Integer -> IO a -> IO ()
runLogger filename delay action = forever $ do
	entry <- action
	date <- getCurrentTime
	createTimeLog False filename
	appendTimeLog filename (TimeLogEntry date delay entry)
	threadDelay (fromIntegral delay * 1000)
	
createTimeLog :: Bool -> FilePath -> IO ()
createTimeLog force filename = do
	ex <- doesFileExist filename
	when (not ex || force) $ BS.writeFile filename magic

appendTimeLog :: Binary a => FilePath -> TimeLogEntry a -> IO ()
appendTimeLog filename tle = BS.appendFile filename $ encode $ tle

writeTimeLog :: Binary a => FilePath -> TimeLog a -> IO ()
writeTimeLog filename tl = do createTimeLog True filename
			      mapM_ (appendTimeLog filename) tl

readTimeLog :: Binary a => FilePath -> IO (TimeLog a)
readTimeLog filename = do
	content <- BS.readFile filename
        return $ runGet start content
  where start = do
  		startString <- getLazyByteString (BS.length magic)
		if startString == magic
		 then go
		 else error $
		 	"Timelog starts with unknown marker " ++
			show (map (chr.fromIntegral) (BS.unpack startString))
        go = do v <- get
		m <- isEmpty
		if m then return [v]
		     else (v :) <$> go

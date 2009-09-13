module TimeLog where

import Control.Applicative
import System.IO
import Data
import Control.Concurrent
import Control.Monad
import Data.Time

-- | Runs the given action each delay milliseconds and appends the TimeLog to the
-- given file.
runLogger :: (Show a) => FilePath -> Integer -> IO a -> IO ()
runLogger filename delay action = forever $ do
	entry <- action
	date <- getCurrentTime
	appendTimeLog filename (TimeLogEntry date delay entry)
	threadDelay (fromIntegral delay * 1000)
	

appendTimeLog :: Show a => FilePath -> TimeLogEntry a -> IO ()
-- Double show to ensure it is one string on one line
appendTimeLog filename tl = appendFile filename $ (++"\n") $ show $ show $ tl

readTimeLog :: Read a => FilePath -> IO (TimeLog a)
readTimeLog filename = (map (read.read) . lines) <$> (openFile filename ReadMode >>= hGetContents)

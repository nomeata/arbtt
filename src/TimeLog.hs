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
import Control.Exception
import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as BS
import Data.Maybe

magic = BS.pack $ map (fromIntegral.ord) "arbtt-timelog-v1\n"

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
appendTimeLog filename = BS.appendFile filename . encode

writeTimeLog :: Binary a => FilePath -> TimeLog a -> IO ()
writeTimeLog filename tl = do createTimeLog True filename
			      mapM_ (appendTimeLog filename) tl

-- | This might be very bad style, and it hogs memory, but it might help in some situations...
recoverTimeLog :: Binary a => FilePath -> IO (TimeLog a)
recoverTimeLog filename = do
	content <- BS.readFile filename
        start content
  where start content = do
  		let (startString, rest, _) = runGetState (getLazyByteString (BS.length magic)) content 0
		if startString /= magic
		  then do putStrLn $ "WARNING: Timelog starts with unknown marker " ++
				show (map (chr.fromIntegral) (BS.unpack startString))
		  else do putStrLn $ "Found header, continuing... (" ++ show (BS.length rest) ++ " bytes to go)"
		go rest
        go input = do mb <- tryGet input
	 	      flip (maybe (return [])) mb $
		     	 \(v,rest,_) -> if BS.null rest then return [v]
			                                else (v :) <$> go rest
	tryGet input = catch (
			do -- putStrLn $ "Trying value at offset " ++ show off
			   let (v,rest,off) = runGetState get input 0
			   evaluate rest
			   return (Just (v,rest,off))
			) (
			\e -> do
			   putStrLn $ "Failed: " ++ show (e :: SomeException)
		    	   if BS.length input <= 1
			     then return Nothing
			     else do putStrLn $ "Failed to read value, retrying with rest"-- ++ show (off+1)
			             tryGet (BS.tail input)
			)

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

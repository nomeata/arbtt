module TimeLog where

import Data

import Control.Applicative
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Time
import Data.Binary
import Data.Binary.StringRef
import Data.Binary.Get
import Data.Function
import Data.Char
import System.Directory
import Control.Exception
import Prelude hiding (catch)
import Control.DeepSeq
import System.Posix.Files
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString.Lazy as BS
import Data.Maybe

magic = BS.pack $ map (fromIntegral.ord) "arbtt-timelog-v1\n"

-- | Runs the given action each delay milliseconds and appends the TimeLog to the
-- given file.
runLogger :: ListOfStringable a => FilePath -> Integer -> IO a -> IO ()
runLogger filename delay action = flip fix Nothing $ \loop prev -> do
        entry <- action
        date <- getCurrentTime
        createTimeLog False filename
        setFileMode filename (ownerReadMode `unionFileModes` ownerWriteMode)
        appendTimeLog filename prev (TimeLogEntry date delay entry)
        threadDelay (fromIntegral delay * 1000)
        loop (Just entry)

        
createTimeLog :: Bool -> FilePath -> IO ()
createTimeLog force filename = do
        ex <- doesFileExist filename
        when (not ex || force) $ BS.writeFile filename magic

appendTimeLog :: ListOfStringable a => FilePath -> Maybe a -> TimeLogEntry a -> IO ()
appendTimeLog filename prev = BS.appendFile filename . ls_encode strs
  where strs = maybe [] listOfStrings prev

writeTimeLog :: ListOfStringable a => FilePath -> TimeLog a -> IO ()
writeTimeLog filename tl = do
        createTimeLog True filename
        foldM_ go  Nothing tl
  where go prev v = do appendTimeLog filename prev v
                       return (Just (tlData v))

-- | This might be very bad style, and it hogs memory, but it might help in some situations...
-- Use of unsafeInterleaveIO should be replaced by conduit, pipe or something the like
recoverTimeLog :: ListOfStringable a => FilePath -> IO (TimeLog a)
recoverTimeLog filename = do
        content <- BS.readFile filename
        start content
  where start content = do
                let (startString, rest, off) = runGetState (getLazyByteString (BS.length magic)) content 0
                if startString /= magic
                  then do putStrLn $ "WARNING: Timelog starts with unknown marker " ++
                                show (map (chr.fromIntegral) (BS.unpack startString))
                  else do putStrLn $ "Found header, continuing... (" ++ show (BS.length rest) ++ " bytes to go)"
                go Nothing rest off

        go prev input off = do
                mb <- tryGet prev input off off
                flip (maybe (return [])) mb $ \(v,rest,off') ->
                        if BS.null rest
                        then return [v]
                        else (v:) <$> (unsafeInterleaveIO $ go (Just (tlData v)) rest off')

        tryGet prev input off orig_off = catch (
                        do -- putStrLn $ "Trying value at offset " ++ show off
                           let (v,rest,off') = runGetState (ls_get strs) input off
                           evaluate rest
                           when (off /= orig_off) $
                                putStrLn $ "Skipped from " ++ show orig_off ++ ", succesful read at position " ++ show off ++ ", lost " ++ show (off - orig_off) ++ " bytes."
                           return (Just (v,rest,off'))
                        ) (
                        \e -> do
                           putStrLn $ "Failed to read value at position " ++ show off ++ ":"
                           putStrLn $ "   " ++ show (e :: SomeException)
                           if BS.length input <= 1
                             then do putStrLn $ "End of file reached"
                                     return Nothing
                             else do tryGet prev (BS.tail input) (off+1) orig_off
                        )
          where strs = maybe [] listOfStrings prev

readTimeLog :: (NFData a, ListOfStringable a) => FilePath -> IO (TimeLog a)
readTimeLog filename = do
        content <- BS.readFile filename
        return $ parseTimeLog content

parseTimeLog :: (NFData a, ListOfStringable a) => BS.ByteString -> TimeLog a
parseTimeLog input = 
    if startString == magic
       then go Nothing rest off
       else error $
            "Timelog starts with unknown marker " ++
            show (map (chr.fromIntegral) (BS.unpack startString))
  where                
    (startString, rest, off) = runGetState (getLazyByteString (BS.length magic)) input 0
    go prev input off =
        let (v, rest, off') = runGetState (ls_get strs) input off
        in v `deepseq`
           if (BS.null rest)
           then [v]
           else v : go (Just (tlData v)) rest off'
      where strs = maybe [] listOfStrings prev


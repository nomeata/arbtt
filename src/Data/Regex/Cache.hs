module Data.Regex.Cache where

import qualified Text.Regex.PCRE.Light.Text as RE
import qualified Data.MyText as T
import qualified Data.Trie as Trie
import Data.IORef
import System.IO.Unsafe
import Debug.Trace


type RegexMatcher = T.Text -> Maybe [T.Text]

cacheRegex :: Monad m => RE.Regex -> m (T.Text -> Maybe [T.Text])
cacheRegex regex = do
    cacheref <- return $! unsafePerformIO (regex `seq` newIORef (Trie.empty))
    return $ \str -> unsafePerformIO $ do
        cache <- readIORef cacheref 
        let key = T.toBytestring str
        case Trie.lookup key cache of
            Nothing -> do
                let res = tail `fmap` RE.match regex str []
                trace ("Evaluating     " ++ show str ++ " against " ++ show regex) (return ())
                modifyIORef cacheref (Trie.insert key res)
                return res
            Just res -> do
                trace ("Not evaluating " ++ show str ++ " against " ++ show regex) (return ())
                return res

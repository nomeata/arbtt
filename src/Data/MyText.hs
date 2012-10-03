module Data.MyText where

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString as BS
import Data.Binary
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Prelude hiding (length, map)
import qualified Prelude
import GHC.Exts( IsString(..) )
import Control.DeepSeq

newtype Text = Text { toBytestring :: BSU.ByteString } deriving (Eq, Ord)

instance Show Text where
    showsPrec i t = showsPrec i (toBytestring t)

instance Read Text where
    readsPrec i s = Prelude.map (first Text) $ readsPrec i s 

instance IsString Text where
    fromString = pack

-- Binary instance compatible with Binary String
instance Binary Text where
    put = put . unpack
    get = pack <$> get

{- Possible speedup with a version of binary that provides access to the
   internals, as the Char instance is actually UTF8, but the length bit is
   chars, not bytes.
instance Binary Text where
    put = put . unpack
    get = do
        n <- get :: Get Int
        let go = do
            s <- GI.get 
            let utf8s = BSU.take n s
            if BSU.length utf8s == n
                then GI.skip (B.length utf8s) >> return utf8s
                else GI.demandInput >> go
        go
-}

instance NFData Text where
    rnf (Text a) = a `seq` ()

length :: Text -> Int
length (Text bs) = BSU.length bs

decodeUtf8 :: BS.ByteString -> Text
decodeUtf8 = Text

encodeUtf8 :: Text -> BS.ByteString
encodeUtf8 = toBytestring

unpack :: Text -> String
unpack = BSU.toString . toBytestring

pack :: String -> Text
pack = Text . BSU.fromString

map :: (Char -> Char) -> Text -> Text
map f = pack . Prelude.map f . unpack

concat :: [Text] -> Text
concat = Text . BS.concat . Prelude.map toBytestring

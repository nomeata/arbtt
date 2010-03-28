module Data.MyText where

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString as BS
import Data.Binary
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Prelude hiding (length, map)
import qualified Prelude
import GHC.Exts( IsString(..) )

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

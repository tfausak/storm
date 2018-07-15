module Storm.Internal.Type.Str
  ( Str(..)
  , strToText
  , decodeStr
  , encodeStr
  ) where

import qualified Data.ByteString.Char8 as Bytes8
import qualified Data.Char as Char
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Storm.Internal.Decode as Decode
import qualified Storm.Internal.Encode as Encode
import qualified Storm.Internal.Json as Json
import qualified Storm.Internal.Type.I32 as I32

newtype Str = Str Text.Text deriving (Eq, Show)

strToText :: Str -> Text.Text
strToText (Str x) = x

instance Json.FromJSON Str where
  parseJSON = fmap Str . Json.parseJSON

instance Json.ToJSON Str where
  toJSON = Json.toJSON . strToText

decodeStr :: Decode.Decoder Str
decodeStr = do
  n <- I32.i32ToInt32 <$> I32.decodeI32
  if n < 0
    then Str . Text.decodeUtf16LE <$> Decode.consume (-2 * int32ToInt n)
    else Str . Text.decodeLatin1 <$> Decode.consume (int32ToInt n)

-- TODO: This might not round-trip properly. Some strings have all Latin-1
-- characters and yet they're encoded as UTF-16.
encodeStr :: Encode.Encoder Str
encodeStr s =
  let t = strToText s
  in if Text.all Char.isLatin1 t
    then I32.encodeI32 (I32.I32 (intToInt32 (Text.length t))) <> Encode.encodeBytes (encodeLatin1 t)
    else I32.encodeI32 (I32.I32 (intToInt32 (-2 * Text.length t))) <> Encode.encodeBytes (Text.encodeUtf16LE t)

encodeLatin1 :: Text.Text -> Bytes8.ByteString
encodeLatin1 = Bytes8.pack . Text.unpack

int32ToInt :: Int.Int32 -> Int
int32ToInt = fromIntegral

intToInt32 :: Int -> Int.Int32
intToInt32 = fromIntegral

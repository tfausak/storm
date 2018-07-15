module Storm.Internal.Type.I32
  ( I32(..)
  , i32ToInt32
  , decodeI32
  , encodeI32
  )
where

import qualified Data.Int as Int
import qualified Storm.Internal.Decode as Decode
import qualified Storm.Internal.Encode as Encode
import qualified Storm.Internal.Json as Json

newtype I32 = I32 Int.Int32 deriving (Eq, Show)

i32ToInt32 :: I32 -> Int.Int32
i32ToInt32 (I32 x) = x

instance Json.FromJSON I32 where
  parseJSON = fmap I32 . Json.parseJSON

instance Json.ToJSON I32 where
  toJSON = Json.toJSON . i32ToInt32

decodeI32 :: Decode.Decoder I32
decodeI32 = I32 <$> Decode.decodeInt32

encodeI32 :: Encode.Encoder I32
encodeI32 = Encode.encodeInt32 . i32ToInt32

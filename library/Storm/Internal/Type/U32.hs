module Storm.Internal.Type.U32
  ( U32(..)
  , u32ToWord32
  , decodeU32
  , encodeU32
  )
where

import qualified Data.Word as Word
import qualified Storm.Internal.Decode as Decode
import qualified Storm.Internal.Encode as Encode
import qualified Storm.Internal.Json as Json

newtype U32 = U32 Word.Word32 deriving (Eq, Show)

u32ToWord32 :: U32 -> Word.Word32
u32ToWord32 (U32 x) = x

instance Json.FromJSON U32 where
  parseJSON = fmap U32 . Json.parseJSON

instance Json.ToJSON U32 where
  toJSON = Json.toJSON . u32ToWord32

decodeU32 :: Decode.Decoder U32
decodeU32 = U32 <$> Decode.decodeWord32

encodeU32 :: Encode.Encoder U32
encodeU32 = Encode.encodeWord32 . u32ToWord32

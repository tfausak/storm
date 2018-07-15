module Storm.Internal.Encode
  ( Encoder
  , runEncoder
  , encodeBytes
  , encodeInt32
  , encodeWord32
  )
where

import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as Builder
import qualified Data.Int as Int
import qualified Data.Word as Word

type Encoder a = a -> Builder.Builder

runEncoder :: Encoder a -> a -> Builder.Builder
runEncoder e = e

encodeBytes :: Encoder Bytes.ByteString
encodeBytes = Builder.byteString

encodeInt32 :: Encoder Int.Int32
encodeInt32 = Builder.int32LE

encodeWord32 :: Encoder Word.Word32
encodeWord32 = Builder.word32LE

module Storm.Internal.Encode
  ( Encoder
  , runEncoder
  , encodeWord32
  )
where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word

type Encoder a = a -> Builder.Builder

runEncoder :: Encoder a -> a -> Builder.Builder
runEncoder e = e

encodeWord32 :: Encoder Word.Word32
encodeWord32 = Builder.word32LE

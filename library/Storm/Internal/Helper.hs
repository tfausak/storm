module Storm.Internal.Helper
  ( generateReplay
  , parseReplay
  )
where

import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as Builder
import qualified Storm.Internal.Decode as Decode
import qualified Storm.Internal.Encode as Encode
import qualified Storm.Internal.Type.Replay as Replay

generateReplay :: Replay.Replay -> Builder.Builder
generateReplay = Encode.runEncoder Replay.encodeReplay

parseReplay :: Bytes.ByteString -> Either String Replay.Replay
parseReplay = fst . Decode.runDecoder Replay.decodeReplay

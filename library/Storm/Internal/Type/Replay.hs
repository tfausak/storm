module Storm.Internal.Type.Replay
  ( Replay(..)
  , decodeReplay
  , encodeReplay
  )
where

import qualified Storm.Internal.Decode as Decode
import qualified Storm.Internal.Encode as Encode
import qualified Storm.Internal.Json as Json
import qualified Storm.Internal.Type.U32 as U32

data Replay = Replay
  { replayUnknown1 :: U32.U32
  , replayUnknown2 :: U32.U32
  , replayUnknown3 :: U32.U32
  , replayUnknown4 :: U32.U32
  , replayUnknown5 :: U32.U32
  } deriving (Eq, Show)

instance Json.FromJSON Replay where
  parseJSON =
    Json.withObject "Replay" $ \o ->
      Replay <$> Json.getKey o "unknown1" <*> Json.getKey o "unknown2" <*>
      Json.getKey o "unknown3" <*>
      Json.getKey o "unknown4" <*>
      Json.getKey o "unknown5"

instance Json.ToJSON Replay where
  toJSON x =
    Json.object
      [ Json.setKey "unknown1" $ replayUnknown1 x
      , Json.setKey "unknown2" $ replayUnknown2 x
      , Json.setKey "unknown3" $ replayUnknown3 x
      , Json.setKey "unknown4" $ replayUnknown4 x
      , Json.setKey "unknown5" $ replayUnknown5 x
      ]

decodeReplay :: Decode.Decoder Replay
decodeReplay =
  Replay
    <$> U32.decodeU32
    <*> U32.decodeU32
    <*> U32.decodeU32
    <*> U32.decodeU32
    <*> U32.decodeU32

encodeReplay :: Encode.Encoder Replay
encodeReplay x =
  U32.encodeU32 (replayUnknown1 x)
    <> U32.encodeU32 (replayUnknown2 x)
    <> U32.encodeU32 (replayUnknown3 x)
    <> U32.encodeU32 (replayUnknown4 x)
    <> U32.encodeU32 (replayUnknown5 x)

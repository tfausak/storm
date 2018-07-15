module Storm.Internal.Type.Replay
  ( Replay(..)
  , decodeReplay
  , encodeReplay
  )
where

import qualified Storm.Internal.Decode as Decode
import qualified Storm.Internal.Encode as Encode
import qualified Storm.Internal.Json as Json
import qualified Storm.Internal.Type.Str as Str
import qualified Storm.Internal.Type.U32 as U32

data Replay = Replay
  { replayUnknown1 :: U32.U32 -- 480436863
  , replayUnknown2 :: U32.U32 -- 5
  , replayUnknown3 :: U32.U32 -- time in milliseconds
  , replayUnknown4 :: U32.U32 -- 2
  , replayUnknown5 :: U32.U32 -- 4204439
  , replayUnknown6 :: Str.Str -- "Unsaved Replay" followed by a bunch of spaces
  , replayUnknown7 :: U32.U32 -- 0
  , replayUnknown8 :: U32.U32 -- apparently random
  , replayUnknown9 :: U32.U32 -- maybe some type of timestamp?
  } deriving (Eq, Show)

instance Json.FromJSON Replay where
  parseJSON =
    Json.withObject "Replay" $ \o -> Replay
      <$> Json.getKey o "unknown1"
      <*> Json.getKey o "unknown2"
      <*> Json.getKey o "unknown3"
      <*> Json.getKey o "unknown4"
      <*> Json.getKey o "unknown5"
      <*> Json.getKey o "unknown6"
      <*> Json.getKey o "unknown7"
      <*> Json.getKey o "unknown8"
      <*> Json.getKey o "unknown9"

instance Json.ToJSON Replay where
  toJSON x =
    Json.object
      [ Json.setKey "unknown1" $ replayUnknown1 x
      , Json.setKey "unknown2" $ replayUnknown2 x
      , Json.setKey "unknown3" $ replayUnknown3 x
      , Json.setKey "unknown4" $ replayUnknown4 x
      , Json.setKey "unknown5" $ replayUnknown5 x
      , Json.setKey "unknown6" $ replayUnknown6 x
      , Json.setKey "unknown7" $ replayUnknown7 x
      , Json.setKey "unknown8" $ replayUnknown8 x
      , Json.setKey "unknown9" $ replayUnknown9 x
      ]

decodeReplay :: Decode.Decoder Replay
decodeReplay =
  Replay
    <$> U32.decodeU32
    <*> U32.decodeU32
    <*> U32.decodeU32
    <*> U32.decodeU32
    <*> U32.decodeU32
    <*> Str.decodeStr
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
    <> Str.encodeStr (replayUnknown6 x)
    <> U32.encodeU32 (replayUnknown7 x)
    <> U32.encodeU32 (replayUnknown8 x)
    <> U32.encodeU32 (replayUnknown9 x)

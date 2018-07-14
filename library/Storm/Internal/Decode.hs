module Storm.Internal.Decode
  ( Decoder
  , runDecoder
  , decodeWord32
  )
where

import qualified Control.Monad.Fail as Fail
import qualified Data.Bits as Bits
import qualified Data.ByteString as Bytes
import qualified Data.Word as Word

newtype Decoder a =
  Decoder (Bytes.ByteString -> (Either String a, Bytes.ByteString))

runDecoder
  :: Decoder a -> Bytes.ByteString -> (Either String a, Bytes.ByteString)
runDecoder (Decoder d) = d

instance Functor Decoder where
  fmap = fmapDecoder

fmapDecoder :: (a -> b) -> Decoder a -> Decoder b
fmapDecoder f dx = Decoder $ \b1 -> case runDecoder dx b1 of
  (Left e, b2) -> (Left e, b2)
  (Right x, b2) -> (Right (f x), b2)

instance Applicative Decoder where
  pure = pureDecoder
  (<*>) = applyDecoder

pureDecoder :: a -> Decoder a
pureDecoder x = Decoder $ \b -> (Right x, b)

applyDecoder :: Decoder (a -> b) -> Decoder a -> Decoder b
applyDecoder df dx = Decoder $ \b1 -> case runDecoder df b1 of
  (Left e, b2) -> (Left e, b2)
  (Right f, b2) -> case runDecoder dx b2 of
    (Left e, b3) -> (Left e, b3)
    (Right x, b3) -> (Right (f x), b3)

instance Monad Decoder where
  (>>=) = bindDecoder
  fail = Fail.fail

bindDecoder :: Decoder a -> (a -> Decoder b) -> Decoder b
bindDecoder dx f = Decoder $ \b1 -> case runDecoder dx b1 of
  (Left e, b2) -> (Left e, b2)
  (Right x, b2) -> runDecoder (f x) b2

instance Fail.MonadFail Decoder where
  fail = failDecoder

failDecoder :: String -> Decoder a
failDecoder e = Decoder $ \b -> (Left e, b)

get :: Decoder Bytes.ByteString
get = Decoder $ \b -> (Right b, b)

put :: Bytes.ByteString -> Decoder ()
put b = Decoder $ const (Right (), b)

consume :: Int -> Decoder Bytes.ByteString
consume n = do
  b <- get
  let (l, r) = Bytes.splitAt n b
  put r
  pure l

decodeWord32 :: Decoder Word.Word32
decodeWord32 = unsafeByteStringToWord32 <$> consume 4

unsafeByteStringToWord32 :: Bytes.ByteString -> Word.Word32
unsafeByteStringToWord32 b =
  word8ToWord32 (Bytes.index b 0)
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index b 1)) 8
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index b 2)) 16
    Bits..|. Bits.shiftL (word8ToWord32 (Bytes.index b 3)) 24

word8ToWord32 :: Word.Word8 -> Word.Word32
word8ToWord32 = fromIntegral

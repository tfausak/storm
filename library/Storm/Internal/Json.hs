module Storm.Internal.Json
  ( Aeson.FromJSON
  , Aeson.ToJSON
  , Aeson.parseJSON
  , Aeson.toJSON
  , Aeson.withObject
  , Aeson.object
  , getKey
  , setKey
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

getKey :: Aeson.FromJSON v => Aeson.Object -> String -> Aeson.Parser v
getKey object key = object Aeson..: Text.pack key

setKey :: (Aeson.ToJSON v, Aeson.KeyValue kv) => String -> v -> kv
setKey key value = Text.pack key Aeson..= value

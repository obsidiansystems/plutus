module Data.RawJson where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Foreign (Foreign, readString, unsafeToForeign)
import Foreign.JSON (parseJSON)
import Foreign.Class (class Decode, class Encode, encode, decode)
import Global.Unsafe (unsafeStringify)
import Control.Monad.Except (lift, runExceptT)
import Data.Either
import Data.Identity

newtype RawJson
  = RawJson Foreign

derive instance genericRawJson :: Generic RawJson _

derive instance newtypeRawJson :: Newtype RawJson _

instance eqRawJson :: Eq RawJson where
  eq _ _ = false -- TODO: Fixme

_RawJson :: Iso' RawJson Foreign
_RawJson = _Newtype

foreign import _pretty :: Foreign -> String

pretty :: RawJson -> String
pretty (RawJson json) = _pretty json

instance showRawJson :: Show RawJson where
  show = pretty

instance encodeRawJson :: Encode RawJson where
  encode (RawJson json) = encode json

instance decodeRawJson :: Decode RawJson where
  decode value = RawJson <$> decode value

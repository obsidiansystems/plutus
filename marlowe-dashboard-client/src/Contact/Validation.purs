module Contact.Validation
  ( KeyError(..)
  , NicknameError(..)
  , keyError
  , nicknameError
  ) where

import Prelude
import Contact.Types (Contact, PubKeyHash)
import Data.Map (Map, member)
import Data.Map.Extra (mapIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)

data NicknameError
  = EmptyNickname
  | DuplicateNickname

derive instance eqNicknameError :: Eq NicknameError

instance showNicknameError :: Show NicknameError where
  show EmptyNickname = "Nickname cannot be blank"
  show DuplicateNickname = "Nickname is already in use in your contacts"

data KeyError
  = EmptyKey
  | DuplicateKey

derive instance eqKeyError :: Eq KeyError

instance showKeyError :: Show KeyError where
  show EmptyKey = "Wallet key cannot be blank"
  show DuplicateKey = "Wallet key is already in your contacts"

nicknameError :: String -> Map (Tuple String PubKeyHash) Contact -> Maybe NicknameError
nicknameError "" _ = Just EmptyNickname

nicknameError nickname contacts =
  if member nickname $ mapIndex fst $ contacts then
    Just DuplicateNickname
  else
    Nothing

keyError :: PubKeyHash -> Map (Tuple String PubKeyHash) Contact -> Maybe KeyError
keyError "" _ = Just EmptyKey

keyError key contacts =
  if member key $ mapIndex snd $ contacts then
    Just DuplicateKey
  else
    Nothing

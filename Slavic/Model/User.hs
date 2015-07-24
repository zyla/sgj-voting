module Slavic.Model.User where

import ClassyPrelude.Slavic
import Slavic.Model
import qualified Crypto.PasswordStore as PS

passwordStrength :: Int
passwordStrength = 15

-- Hash a password.
-- IO is used to generate the salt.
makePassword :: Text -> IO ByteString
makePassword = flip PS.makePassword passwordStrength . encodeUtf8

-- Verify if a password matches given hashed password.
verifyPassword :: Text -> ByteString -> Bool
verifyPassword input hashedPassword = PS.verifyPassword (encodeUtf8 input) hashedPassword


makeUser
    :: TokenId
    -> Text -- ^ nick
    -> Text -- ^ password, not hashed
    -> Text -- ^ fist name
    -> Text -- ^ last name
    -> Text -- ^ city
    -> IO User
makeUser token nick password firstName lastName city = do
    hashedPassword <- makePassword password
    pure $ User token nick hashedPassword firstName lastName city Nothing

login :: MonadIO m
      => Text -- ^ nick
      -> Text -- ^ password
      -> SqlPersistT m (Either Text (Entity User))

login nick password = runExceptT $ do
    user <- (lift $ getBy $ UniqueUserNick nick) `orThrow` errorMsg
    unless (verifyPassword password $ userPassword $ entityVal user) $ throwError errorMsg
    return user

  where
    errorMsg = "Invalid nick/password"


-- | Functions for working with passwords and PINs.
-- Uses Crypto.PasswordStore behind the scenes.
module Slavic.Model.Password where

import ClassyPrelude
import qualified Crypto.PasswordStore as PS
import Database.Persist.Sql

passwordStrength :: Int
passwordStrength = 15

-- Hash a password.
-- IO is used to generate the salt.
makePassword :: Text -> IO ByteString
makePassword = flip PS.makePassword passwordStrength . encodeUtf8

-- Verify if a password matches given hashed password.
verifyPassword :: Text -> ByteString -> Bool
verifyPassword input hashedPassword = PS.verifyPassword (encodeUtf8 input) hashedPassword

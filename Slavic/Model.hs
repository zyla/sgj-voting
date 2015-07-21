module Slavic.Model (
    module Slavic.Model
  , module Database.Persist.Sql
) where

import ClassyPrelude
import Database.Persist.Sql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Token
        token Text

    User
        token     Text
        nick      Text
        password  Text
        firstName Text
        lastName  Text
        city      Text
        deriving Show
|]

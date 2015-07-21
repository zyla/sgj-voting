module Slavic.Model (
    module Slavic.Model
  , module Database.Persist.Sql
) where

import ClassyPrelude
import Database.Persist.Sql
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User
        token Text
|]

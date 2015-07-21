-- | This module joins all the handlers together.
module Slavic (
    module Slavic
  , module Slavic.Foundation
) where

import ClassyPrelude
import Yesod
import Database.Persist.Sql
import Database.Persist.Postgresql
import Control.Monad.Logger (runStderrLoggingT)
import Slavic.Model
import Slavic.Foundation

import Slavic.Handler.Root

mkYesodDispatch "App" resourcesApp

makeApp :: IO App
makeApp = runStderrLoggingT $ do
    pool <- openConnectionPool
    runSqlPool (runMigration migrateAll) pool
    return $ App pool
  where
    openConnectionPool = createPostgresqlPool connString poolSize
    connString = "dbname=slavic"
    poolSize = 10

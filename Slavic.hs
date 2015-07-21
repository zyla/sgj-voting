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
import Slavic.Handler.Registration

mkYesodDispatch "App" resourcesApp

makeApp :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) => m App
makeApp = do
    pool <- openConnectionPool
    runSqlPool (runMigration migrateAll) pool
    return $ App pool
  where
    openConnectionPool = createPostgresqlPool connString poolSize
    connString = "dbname=slavic"
    poolSize = 10

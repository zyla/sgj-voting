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
import Slavic.Handler.Login

mkYesodDispatch "App" resourcesApp

makeApp :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) => ByteString -> m App
makeApp connString = do
    pool <- openConnectionPool
    runSqlPool (runMigration migrateAll) pool
    return $ App pool
  where
    openConnectionPool = createPostgresqlPool connString poolSize
    poolSize = 10

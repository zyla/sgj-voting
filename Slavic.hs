-- | This module joins all the handlers together.
module Slavic (
    module Slavic
  , module Slavic.Foundation
) where

import ClassyPrelude
import Yesod
import Yesod.Static (static, staticDevel)
import Yesod.Core.Types (Logger(loggerSet))
import Database.Persist.Sql
import Database.Persist.Postgresql
import Control.Monad.Logger (runStderrLoggingT)
import Slavic.Model
import Slavic.Foundation

import Slavic.Handler.Root
import Slavic.Handler.Registration
import Slavic.Handler.Login
import Slavic.Handler.Team

import Network.Wai.Middleware.RequestLogger

import Data.Default

mkYesodDispatch "App" resourcesApp

makeApp :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) => ByteString -> m App
makeApp connString = do
    appStatic <- liftIO $ staticDevel "static"
    pool <- openConnectionPool
    runSqlPool (runMigration migrateAll) pool
    return $ App pool appStatic
  where
    openConnectionPool = createPostgresqlPool connString poolSize
    poolSize = 10

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
-- Stolen from Yesod scafoolding
toWaiApp' :: App -> IO Application
toWaiApp' app = do
    logger <- makeLogger app
    logWare <- mkRequestLogger def
        { outputFormat = Apache FromFallback
        , destination = Logger $ loggerSet logger
        }

    logWare <$> defaultMiddlewaresNoLogging <$> toWaiAppPlain app

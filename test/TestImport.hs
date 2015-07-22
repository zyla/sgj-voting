module TestImport
    ( module TestImport
    , module X
    ) where

import ClassyPrelude         as X
import Slavic.Foundation     as X
import Yesod.Test            as X
import Test.Hspec            as X
import Slavic (makeApp)
import Text.RawString.QQ (r)

import Database.Persist.Sql
import Control.Monad.Logger

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnectionPool app)

withApp :: SpecWith App -> Spec
withApp = before $ do
    app <- runNoLoggingT $ makeApp "dbname=slavic_test"
    wipeDB app
    return app

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    when (not . null $ tables) $
        let escapedTables = map (connEscapeName sqlBackend . DBName) tables
            query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
        in rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [r|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- Utilities

-- Alias, not to confuse CSRF tokens with our tokens
addCSRFToken :: RequestBuilder site ()
addCSRFToken = addToken

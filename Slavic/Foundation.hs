module Slavic.Foundation where

import ClassyPrelude
import Yesod
import Yesod.Auth hiding (LoginR)
import Text.Hamlet
import Database.Persist.Sql
import Yesod.Static (Static, staticFiles)

data App = App
    { appConnectionPool :: ConnectionPool
    , appStatic :: Static
    }

mkYesodData "App" [parseRoutes|
    / RootR GET
    /static StaticR Static appStatic
    /register RegisterR GET POST
    /login LoginR GET POST
|]

staticFiles "static"

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/main.hamlet")

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        app <- getYesod
        runSqlPool action $ appConnectionPool app

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


instance YesodAuth App where
    type AuthId App = Text

    loginDest _ = RootR
    logoutDest _ = RootR

    maybeAuthId = lookupSession credsKey 

    authPlugins _ = []

    authHttpManager _ = error "This app doesn't neet HTTP manager"

    getAuthId = return . Just . credsIdent

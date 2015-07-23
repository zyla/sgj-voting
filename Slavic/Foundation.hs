module Slavic.Foundation where

import ClassyPrelude
import Yesod
import Yesod.Auth hiding (LoginR)
import Database.Persist.Sql

data App = App
    { appConnectionPool :: ConnectionPool
    }

mkYesodData "App" [parseRoutes|
    / RootR GET
    /register RegisterR GET POST
    /login LoginR GET POST
|]

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

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

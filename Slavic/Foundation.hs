module Slavic.Foundation where

import ClassyPrelude
import Yesod
import Database.Persist.Sql

data App = App
    { appConnectionPool :: ConnectionPool
    }

mkYesodData "App" [parseRoutes|
    / RootR GET
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

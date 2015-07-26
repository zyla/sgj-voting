module Slavic.Foundation where

import ClassyPrelude
import Yesod
import Yesod.Auth hiding (LoginR, LogoutR)
import Text.Hamlet
import Database.Persist.Sql
import Yesod.Static (Static, staticFiles)

import Slavic.Model

data App = App
    { appConnectionPool :: ConnectionPool
    , appStatic :: Static
    }

mkYesodData "App" [parseRoutes|
    / RootR GET
    /static StaticR Static appStatic
    /register RegisterR GET POST
    /register-thank-you RegisterSuccessfulR GET
    /login LoginR GET POST
    /logout LogoutR GET
    /add-team AddTeamR GET POST
    /leave-team LeaveTeamR GET POST
    /teams/#TeamId/join JoinTeamR POST
|]

staticFiles "static"

instance Yesod App where
    defaultLayout widget = do
        maybeAuthUser <- getAuthUser
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

    authHttpManager _ = error "This app doesn't need HTTP manager"

    getAuthId = return . Just . credsIdent

getAuthUser :: Handler (Maybe (Entity User))
getAuthUser = maybeAuthId >>= \case
    Just authId -> runDB $ getBy $ UniqueUserNick authId
    Nothing -> return Nothing

setAuthUserNick :: Text -> Handler ()
setAuthUserNick = setSession credsKey

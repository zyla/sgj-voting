module Slavic.Foundation (
    module Slavic.Foundation
  , requireAuth
) where

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
    /teams/#TeamId TeamR GET
    /edit_game EditGameR GET POST
    /vote VoteR GET POST
|]

staticFiles "static"

instance Yesod App where
    defaultLayout widget = do
        maybeAuthUser <- getAuthUser
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/main.hamlet")

    authRoute _ = Just LoginR

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        app <- getYesod
        runSqlPool action $ appConnectionPool app

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = RootR
    logoutDest _ = RootR

    authPlugins _ = []

    authHttpManager _ = error "This app doesn't need HTTP manager"

    getAuthId = runDB . map (map entityKey) . getBy . UniqueUserNick . credsIdent

instance YesodAuthPersist App

getAuthUser :: Handler (Maybe (Entity User))
getAuthUser = map (uncurry Entity) <$> maybeAuthPair

setAuthUserNick :: Text -> Handler ()
setAuthUserNick = setCreds False . flip (Creds "") []

runSqlMEither :: Show e => SqlM (Either e a) -> Handler a
runSqlMEither a = runDB a >>= either (invalidArgs . return . tshow) return


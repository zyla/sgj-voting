module Slavic.Handler.Login where

import ClassyPrelude
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.Password

data LoginRequest = LoginRequest
    { lr_login :: Text
    , lr_password :: Text
    }

loginForm :: Html -> MForm Handler (FormResult LoginRequest, Widget)
loginForm = renderDivs $ LoginRequest
    <$> areq textField (fs "Login" "login") Nothing
    <*> areq passwordField (fs "Password" "password") Nothing
  where
    fs label name = (fieldSettingsFromLabel label) { fsName = Just name, fsId = Just name }
    fieldSettingsFromLabel = fromString

displayLoginForm :: Widget -> Enctype -> Handler Html
displayLoginForm widget enctype =
    defaultLayout [whamlet|
        <form method=post action=@{LoginR} enctype=#{enctype}>
            ^{widget}
            <input type=submit>
        |]

getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost loginForm
    displayLoginForm widget enctype

postLoginR :: Handler Html
postLoginR = do
    ((result,widget), enctype) <- runFormPost loginForm
    case result of
        FormSuccess loginRequest -> do
            result <- login loginRequest
            case result of
                Right _user -> redirect RootR -- TODO: redirect to success page
                Left errors -> liftIO (print errors) >> displayLoginForm widget enctype
        FormFailure errors -> liftIO (print errors) >> displayLoginForm widget enctype
        FormMissing -> displayLoginForm widget enctype

login :: LoginRequest -> Handler (Either Text User)
login LoginRequest{..} = do
    maybeUser <- runDB $ getBy $ UniqueUserNick lr_login
    case maybeUser of
        Nothing -> return $ Left "Invalid login"
        Just (Entity _ user) -> return $
            if verifyPassword lr_password $ userPassword user
            then Right user
            else Left "xD"


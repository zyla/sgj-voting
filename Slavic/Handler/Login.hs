module Slavic.Handler.Login where

import ClassyPrelude.Slavic
import Yesod
import Yesod.Auth hiding (LoginR)
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User
import Slavic.Forms

data LoginRequest = LoginRequest
    { lr_nick :: Text
    , lr_password :: Text
    }

loginForm :: Html -> MForm Handler (FormResult LoginRequest, Widget)
loginForm = renderTable $ LoginRequest
    <$> areq textField (mkFieldSettings "Nick" "nick") Nothing
    <*> areq passwordField (mkFieldSettings "Password" "password") Nothing

displayLoginForm :: Widget -> Enctype -> Handler Html
displayLoginForm widget enctype =
    defaultLayout $ do
        setTitle "Login - SGJ"
        [whamlet|
            <form method=post action=@{LoginR} enctype=#{enctype}>
                <table>^{widget}
                <input type=submit>
        |]

getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost loginForm
    displayLoginForm widget enctype

postLoginR :: Handler Html
postLoginR = do
    ((result,widget), enctype) <- runFormPost loginForm

    let displayErrors errors = displayLoginForm widget' enctype
          where
            widget' = widget >> errorsWidget
            errorsWidget = [whamlet|
                <div .errors>
                    $forall err <- errors
                        <p>#{err}
                |]

    case result of
        FormSuccess LoginRequest{..} -> do
            result <- runDB $ login lr_nick lr_password
            case result of
                Right (Entity _ User{userNick=nick}) -> do
                    setAuthUserNick nick
                    redirect RootR
                Left error -> displayErrors [error]
        FormFailure errors -> displayErrors errors
        FormMissing -> displayLoginForm widget enctype


module Slavic.Handler.Registration where

import ClassyPrelude
import Yesod
import Slavic.Foundation
import Slavic.Model
import Control.Monad.Except (runExceptT, throwError)

-- Using 'lift' exported from ClassyPrelude in fetchToken gets me a weird
-- error, need to investigate this further
import Control.Monad.Trans.Class as T (lift)


registrationForm :: Html -> MForm Handler (FormResult User, Widget)
registrationForm = renderDivs $ User
    <$> (entityKey <$> areq tokenField (fs "Token" "token") Nothing)
    <*> areq textField (fs "Nick" "nick") Nothing
    <*> (encodeUtf8 <$> areq passwordField (fs "Password" "password") Nothing)
    <*> areq textField (fs "First name" "first_name") Nothing
    <*> areq textField (fs "Last name" "last_name") Nothing
    <*> areq textField (fs "City" "city") Nothing
  where
    fs label name = (fieldSettingsFromLabel label) { fsName = Just name, fsId = Just name }
    fieldSettingsFromLabel = fromString

    tokenField = checkMMap fetchToken (tokenToken . entityVal) textField
    fetchToken tokenStr = runDB $ runExceptT $ do
        token <- (T.lift $ getBy $ UniqueToken tokenStr) >>=
            maybe (throwError ("Invalid token" :: Text)) return

        -- Check if token is already registered
        alreadyRegisteredUser <- T.lift $ getBy $ UniqueUserToken $ entityKey token
        when (isJust alreadyRegisteredUser) $
            throwError ("Token already registered" :: Text)

        return token

getRegisterR :: Handler Html
getRegisterR = do
    (widget, enctype) <- generateFormPost registrationForm
    displayRegistrationForm widget enctype

postRegisterR :: Handler Html
postRegisterR = do
    ((result,widget), enctype) <- runFormPost registrationForm
    case result of
        FormSuccess user -> do
            registerUser user
            redirect RootR -- TODO: redirect to success page
        FormFailure errors -> displayRegistrationForm widget enctype

displayRegistrationForm :: Widget -> Enctype -> Handler Html
displayRegistrationForm widget enctype = 
    defaultLayout [whamlet|
        <form method=post action=@{RegisterR} enctype=#{enctype}>
            ^{widget}
            <input type=submit>
        |]

registerUser :: User -> Handler ()
registerUser = runDB . insert_

module Slavic.Handler.Registration where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User

data UserRegistration = UserRegistration
    { urd_token :: TokenId
    , urd_nick :: Text
    , urd_password :: Text
    , urd_firstName :: Text
    , urd_lastName :: Text
    , urd_city :: Text
    }

registrationForm :: Html -> MForm Handler (FormResult UserRegistration, Widget)
registrationForm = renderTable $ UserRegistration
    <$> (entityKey <$> areq tokenField (fs "Token" "token") Nothing)
    <*> areq nickField (fs "Nick" "nick") Nothing
    <*> areq passwordField (fs "Password" "password") Nothing
    <*> areq textField (fs "First name" "first_name") Nothing
    <*> areq textField (fs "Last name" "last_name") Nothing
    <*> areq textField (fs "City" "city") Nothing
  where
    fs label name = (fieldSettingsFromLabel label) { fsName = Just name, fsId = Just name }
    fieldSettingsFromLabel = fromString

    tokenField = checkMMap fetchToken (tokenToken . entityVal) textField

    fetchToken :: Text -> Handler (Either Text (Entity Token))
    fetchToken tokenStr = runDB $ runExceptT $ do
        token <- (lift $ getBy $ UniqueToken tokenStr) `orThrow` "Invalid token"

        -- Check if token is already registered
        alreadyRegisteredUser <- lift $ getBy $ UniqueUserToken $ entityKey token
        when (isJust alreadyRegisteredUser) $
            throwError "Token already registered"

        return token

    nickField = checkM validateUniqueNick textField

    validateUniqueNick :: Text -> Handler (Either Text Text)
    validateUniqueNick nick = (runDB $ getBy $ UniqueUserNick nick) <&> \case
        Nothing -> Right nick
        Just _otherUser -> Left "Nick already registered"

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
            redirect RegisterSuccessfulR -- TODO: redirect to success page
        FormFailure _errors -> displayRegistrationForm widget enctype
        FormMissing -> displayRegistrationForm widget enctype

displayRegistrationForm :: Widget -> Enctype -> Handler Html
displayRegistrationForm widget enctype =
    defaultLayout $ do
    setTitle "Register - SGJ"
    [whamlet|
        <form method=post action=@{RegisterR} enctype=#{enctype}>
            <table>^{widget}
            <input type=submit value="Submit">
        |]


registerUser :: UserRegistration -> Handler ()
registerUser UserRegistration{..} =
    runDB $ insert_ =<< liftIO
        (makeUser urd_token urd_nick urd_password urd_firstName urd_lastName urd_city)

getRegisterSuccessfulR :: Handler Html
getRegisterSuccessfulR = do
    defaultLayout $ do
        setTitle "Registration completed"
        $(whamletFile "templates/registration_successful.hamlet")

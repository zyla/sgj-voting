module Slavic.Handler.Registration where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model


registrationForm :: Html -> MForm Handler (FormResult User, Widget)
registrationForm = renderDivs $ User
    <$> (entityKey <$> areq tokenField (fs "Token" "token") Nothing)
    <*> areq nickField (fs "Nick" "nick") Nothing
    <*> (encodeUtf8 <$> areq passwordField (fs "Password" "password") Nothing)
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
            redirect RootR -- TODO: redirect to success page
        FormFailure _errors -> displayRegistrationForm widget enctype
        FormMissing -> displayRegistrationForm widget enctype

displayRegistrationForm :: Widget -> Enctype -> Handler Html
displayRegistrationForm widget enctype = 
    defaultLayout [whamlet|
        <form method=post action=@{RegisterR} enctype=#{enctype}>
            ^{widget}
            <input type=submit>
        |]

registerUser :: User -> Handler ()
registerUser = runDB . insert_

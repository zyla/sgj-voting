module Slavic.Handler.Registration where

import ClassyPrelude
import Yesod
import Slavic.Foundation
import Slavic.Model


registrationForm :: Html -> MForm Handler (FormResult User, Widget)
registrationForm = renderDivs $ User
    <$> areq textField (fs "Token" "token") Nothing
    <*> areq textField (fs "Nick" "nick") Nothing
    <*> areq passwordField (fs "Password" "password") Nothing
    <*> areq textField (fs "First name" "first_name") Nothing
    <*> areq textField (fs "Last name" "last_name") Nothing
    <*> areq textField (fs "City" "city") Nothing
  where
    fs label name = (fieldSettingsFromLabel label) { fsName = Just name, fsId = Just name }
    fieldSettingsFromLabel = fromString

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

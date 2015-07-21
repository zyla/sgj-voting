module Slavic.Handler.Root where

import ClassyPrelude
import Yesod
--import Yesod.Form.Bootstrap3
import Slavic.Foundation
import Slavic.Model

getRootR :: Handler Html
getRootR = defaultLayout $ do
    setTitle "Slavic Game Jam"
    [whamlet|<p>Hello world|]


registrationForm :: Html -> MForm Handler (FormResult User, Widget)
registrationForm = renderDivs $ User
    <$> areq textField "Token" Nothing
    <*> areq textField "Nick" Nothing
    <*> areq passwordField "Password" Nothing
    <*> areq textField "First name" Nothing
    <*> areq textField "Last name" Nothing
    <*> areq textField "City" Nothing

getRegisterR :: Handler Html
getRegisterR = do
    (widget, enctype) <- generateFormPost registrationForm
    defaultLayout [whamlet|
        <form method=post action=@{RegisterR} enctype=#{enctype}>
            ^{widget}
            <input type=submit>
        |]

postRegisterR :: Handler Html
postRegisterR = do
    ((result,widget), enctype) <- runFormPost registrationForm
    case result of
        FormSuccess user -> defaultLayout [whamlet|<p>#{show user}|]
        _ -> defaultLayout [whamlet|
                <form method=post action=@{RegisterR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>
                |]


module Slavic.Handler.Util where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User

withAuthUser :: (Entity User -> Handler Html) -> Handler Html
withAuthUser handler = do
    maybeAuthUser <- getAuthUser
    case maybeAuthUser of
        Just user -> handler user
        Nothing   -> redirect LoginR


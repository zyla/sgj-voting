module Slavic.Handler.Root where

import ClassyPrelude
import Yesod
import Slavic.Foundation
import Slavic.Model

import Slavic.Handler.Team (displayTeams)

getRootR :: Handler Html
getRootR = 
    getAuthUser >>= \case
        Nothing -> defaultLayout $ do
            setTitle "Slavic Game Jam"
            $(whamletFile "templates/root_anonymous.hamlet")
        Just _authUser -> displayTeams

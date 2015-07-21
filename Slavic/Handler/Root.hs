module Slavic.Handler.Root where

import ClassyPrelude
import Yesod
import Slavic.Foundation
import Slavic.Model

getRootR :: Handler Html
getRootR = defaultLayout $ do
    setTitle "Slavic Game Jam"
    [whamlet|<p>Hello world|]

module Slavic.Handler.Root where

import ClassyPrelude
import Yesod
import Slavic.Foundation

getRootR :: Handler Html
getRootR = defaultLayout $ do
    setTitle "My title"
    [whamlet|<p>Hello world|]

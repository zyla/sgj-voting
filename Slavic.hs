-- | This module joins all the handlers together.
module Slavic (
    module Slavic
  , module Slavic.Foundation
) where

import ClassyPrelude
import Yesod
import Slavic.Foundation

import Slavic.Handler.Root

mkYesodDispatch "App" resourcesApp

module Slavic.Model.Team where

import ClassyPrelude.Slavic
import Slavic.Model

addUserToTeam :: MonadIO m => Team -> Key User -> SqlPersistT m ()
addUserToTeam teamId userId = update userId [UserTeam =. Just teamId]

module Slavic.Model.Team where

import ClassyPrelude.Slavic
import Slavic.Model

addUserToTeam :: MonadIO m => Team -> Key User -> SqlPersistT m ()
addUserToTeam teamId userId = update userId [UserTeam =. Just teamId]

removeUserFromTeam :: MonadIO m => Key User -> SqlPersistT m ()
removeUserFromTeam userId = update userId [UserTeam =. Nothing]

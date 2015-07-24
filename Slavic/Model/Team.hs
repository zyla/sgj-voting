module Slavic.Model.Team where

import ClassyPrelude.Slavic
import Slavic.Model

type SqlM a = MonadIO m => SqlPersistT m a

addUserToTeam :: TeamId -> Key User -> SqlM ()
addUserToTeam teamId userId = update userId [UserTeam =. Just teamId]

removeUserFromTeam :: Key User -> SqlM ()
removeUserFromTeam userId = update userId [UserTeam =. Nothing]

-- | Liat all teams in alphabetic order
getTeams :: SqlM [Entity Team]
getTeams = selectList [] [ Asc TeamName ]

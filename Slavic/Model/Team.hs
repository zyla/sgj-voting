module Slavic.Model.Team where

import ClassyPrelude.Slavic
import Slavic.Model

import Text.RawString.QQ (r)

addUserToTeam :: TeamId -> Key User -> SqlM ()
addUserToTeam teamId userId = update userId [UserTeam =. Just teamId]

removeUserFromTeam :: Key User -> SqlM ()
removeUserFromTeam userId = update userId [UserTeam =. Nothing]

data TeamWithMembers = TeamWithMembers
    { teamWithMembersId :: TeamId
    , teamWithMembersName :: Text
    , teamWithMembersNicks :: [Text]
    } deriving (Eq, Show)

-- | Liat all teams with members in alphabetic order
getTeams :: SqlM [TeamWithMembers]
getTeams = fmap project <$> rawSql [r|
        SELECT team.id, team.name, COALESCE(array_agg(COALESCE(u.nick, '')), ARRAY[]::text[])
        FROM team
        LEFT JOIN "user" u ON u.team = team.id
        GROUP BY team.id, team.name
    |] []
  where
    project (Single teamId, Single teamName, Single nicks) =
        TeamWithMembers teamId teamName nicks


module Slavic.Handler.Vote where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User
import Slavic.Model.Team (TeamWithMembers(..), addUserToTeam, removeUserFromTeam, getTeams)
import Slavic.Model.Voting
import Slavic.Handler.Util (withAuthUser)

getVoteR :: Handler Html
getVoteR = withAuthUser $ \(Entity _ authUser) -> do
    case userTeam authUser of
        Nothing -> redirect RootR
        Just team -> do
            bucket <- runSqlMEither $ getTeamBucket team
            games <- runDB $ getGamesFromBucket $ entityKey bucket
            currentTeam <- case userTeam authUser of
                Nothing -> return Nothing
                Just teamId -> runDB $ fmap (Entity teamId) <$> get teamId
            defaultLayout $ do
                setTitle "Voting - Slavic Game Jam"
                $(whamletFile "templates/voting.hamlet")

postVoteR :: Handler Html
postVoteR = defaultLayout $ return ()

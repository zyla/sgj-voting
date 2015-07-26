module Slavic.Handler.Vote where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User
import Slavic.Model.Team (TeamWithMembers(..), addUserToTeam, removeUserFromTeam, getTeams)
import Slavic.Model.Voting
import Slavic.Handler.Util (withAuthUser)
import Text.Blaze (preEscapedText)

getVoteR :: Handler Html
getVoteR = withAuthUser $ \(Entity userId authUser) -> do
    case userTeam authUser of
        Nothing -> redirect RootR
        Just team -> do
            Entity bucketId bucket <- runSqlMEither $ getTeamBucket team
            games <- runDB $ getGamesFromBucket userId bucketId
            currentTeam <- case userTeam authUser of
                Nothing -> return Nothing
                Just teamId -> runDB $ fmap (Entity teamId) <$> get teamId
            defaultLayout $ do
                setTitle "Voting - Slavic Game Jam"
                let renderOptions val = preEscapedText $ concat $ map (\i ->
                        concat [ "<option val=\"", tshow i, "\" "
                             , if i==val then "selected" else ""
                             , ">", tshow i, "</option>"]) [1,2,3,4,5]
                $(whamletFile "templates/voting.hamlet")

postVoteR :: Handler Html
postVoteR = withAuthUser $ \userE@(Entity _ user) -> do
    case userTeam user of
        Nothing -> redirect RootR
        Just team -> do
            formTeamId <- lookupPostParam "teamId"
            case formTeamId of
                Nothing -> redirect VoteR
                Just formTeamId_ ->
                    let teamId = toSqlKey . readUnsafe $ formTeamId_
                    in if team == teamId
                    then redirect VoteR
                    else do
                        let vote_ cat field = do
                                cat_val <- lookupPostParam field
                                case cat_val of
                                    Nothing -> redirect VoteR
                                    Just val -> runSqlMEither $ vote userE teamId cat (readUnsafe val)
                        _ <- vote_ ZgodnoscZTematem "cat1"
                        _ <- vote_ Jakosc "cat2"
                        _ <- vote_ Innowacyjnosc "cat3"
                        _ <- vote_ Grywalnosc "cat4"
                        redirect VoteR

readUnsafe :: Read a => Text -> a
readUnsafe str = case readMay str of
    Just a -> a
    Nothing -> error $ show str

module Slavic.Model.Voting where

import ClassyPrelude.Slavic
import Slavic.Model
import Text.RawString.QQ (r)

type Round = Int

data VotingError =
      NoCurrentRound
    | NoBucket
    deriving Show

getCurrentRound :: SqlM (Maybe Round)
getCurrentRound = map (currentRoundRound . entityVal) <$> selectFirst [] []

getTeamBucket :: TeamId -> SqlM (Either VotingError (Entity VotingBucket))
getTeamBucket teamId = runExceptT $ do
    round <- lift getCurrentRound `orThrow` NoCurrentRound
    project =<< lift (rawSql [r|
        SELECT vb.id, vb.name, vb.round FROM voting_bucket vb
        INNER JOIN voting_bucket_team vbt ON vb.id = vbt.bucket
        WHERE vbt.team = ? AND vb.round = ?
        |] [toPersistValue teamId, toPersistValue round])
    where project ((Single id, Single name, Single round):_) = return $ Entity id (VotingBucket name round)
          project [] = throwError NoBucket


data TeamWithGame = TeamWithGame
  { twg_id :: TeamId
  , twg_name :: Text
  , twg_game :: Game
  }

-- | Liat all teams with game in alphabetic order
getGamesFromBucket :: VotingBucketId -> SqlM [TeamWithGame]
getGamesFromBucket bucket = fmap project <$> rawSql [r|
        SELECT team.id, team.name, team.game
        FROM team
        INNER JOIN voting_bucket vb ON team.id = vb.team
        WHERE vb.id = ?
    |] [toPersistValue bucket]
  where
    project (Single teamId, Single teamName, Single game) =
        TeamWithGame teamId teamName game

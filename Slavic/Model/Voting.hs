module Slavic.Model.Voting where

import ClassyPrelude.Slavic
import Slavic.Model
import Text.RawString.QQ (r)

type Round = Int

data VotingError =
    NoCurrentRound -- ^ User attempted to vote, but there's no voting round in progress.
  | NoBucket -- ^ User is not in any voting bucket
    deriving (Eq, Show)

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

getGameBucket :: TeamId -> SqlM (Either VotingError (Entity VotingBucket))
getGameBucket teamId = runExceptT $ do
    round <- lift getCurrentRound `orThrow` NoCurrentRound
    project =<< lift (rawSql [r|
        SELECT vb.id, vb.name, vb.round FROM voting_bucket vb
        INNER JOIN voting_bucket_game vbt ON vb.id = vbt.bucket
        WHERE vbt.team = ? AND vb.round = ?
        |] [toPersistValue teamId, toPersistValue round])
    where project ((Single id, Single name, Single round):_) = return $ Entity id (VotingBucket name round)
          project [] = throwError NoBucket


data TeamWithGame = TeamWithGame
  { twg_id :: TeamId
  , twg_name :: Text
  , twg_game :: Game
  , twg_cat1 :: Int
  , twg_cat2 :: Int
  , twg_cat3 :: Int
  , twg_cat4 :: Int
  }

-- | Liat all teams with game in alphabetic order
getGamesFromBucket :: UserId -> VotingBucketId -> SqlM [TeamWithGame]
getGamesFromBucket user bucket = fmap project <$> (rawSql [r|
        SELECT team.id, team.name, team.game,
              coalesce((select value from vote where "owner"=? and "bucket"=? and "game"=team.id and "category"='ZgodnoscZTematem'), 1),
              coalesce((select value from vote where "owner"=? and "bucket"=? and "game"=team.id and "category"='Jakosc'), 1),
              coalesce((select value from vote where "owner"=? and "bucket"=? and "game"=team.id and "category"='Innowacyjnosc'), 1),
              coalesce((select value from vote where "owner"=? and "bucket"=? and "game"=team.id and "category"='Grywalnosc'), 1)
        FROM team
        INNER JOIN voting_bucket_game vbt ON team.id = vbt.team
        WHERE vbt.bucket = ?
    |] $ (mconcat $ replicate 4 [toPersistValue user, toPersistValue bucket]) ++ [toPersistValue bucket])
  where
    project (Single teamId, Single teamName, Single game, Single c1, Single c2, Single c3, Single c4) =
        TeamWithGame teamId teamName game c1 c2 c3 c4

-- | Place a vote. If user has already voted for this game in this category and
-- current round, the vote is updated.
vote :: Entity User -- ^ who votes
     -> TeamId -- ^ for which game
     -> Category
     -> Int -- ^ vote value
     -> SqlM (Either VotingError (Entity Vote))
vote (Entity userId user) gameTeamId category value = runExceptT $ do
    _ <- lift getCurrentRound `orThrow` NoCurrentRound
    userTeamId <- pure (userTeam user) `orThrow` NoBucket
    userBucket <- lift (getTeamBucket userTeamId) >>= either throwError return
    gameBucket <- lift (getGameBucket gameTeamId) >>= either throwError return

    when (entityKey userBucket /= entityKey gameBucket) $ throwError NoBucket

    let vote = Vote userId value gameTeamId (entityKey userBucket) category
    lift $ upsert vote [VoteValue =. value]

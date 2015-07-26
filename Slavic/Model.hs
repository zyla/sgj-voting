module Slavic.Model (
    module Slavic.Model
  , module Database.Persist.Sql
) where

import ClassyPrelude
import Database.Persist.Sql
import Database.Persist.TH
import Slavic.Model.Category

type SqlM a = (Functor m, Monad m, MonadIO m) => SqlPersistT m a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Token
        token Text
        UniqueToken token

    User
        token     TokenId
        nick      Text
        password  ByteString
        firstName Text
        lastName  Text
        city      Text
        team      TeamId Maybe default Nothing

        UniqueUserToken token
        UniqueUserNick nick
        deriving Eq Show

    Team
        name Text
        game GameId Maybe sql=game_id
        UniqueTeamName name

        gameTitle Text Maybe MigrationOnly

        -- Kept for safer migration
        oldGame Game Maybe MigrationOnly sql=game

        deriving Eq Show

    Game
        title  Text
--        screenshot Text
        UniqueGameTitle title
        deriving Eq Show

    VotingBucket
        name  Text
        round Int

    VotingBucketTeam
        team   Team
        bucket VotingBucket

    VotingBucketGame
        game   Game
        bucket VotingBucket

    Vote
        owner    User
        value    Int
        game     Game
        bucket   VotingBucket
        category Category
|]

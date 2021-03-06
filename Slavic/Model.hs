{-# LANGUAGE DeriveDataTypeable #-}
module Slavic.Model (
    module Slavic.Model
  , module Slavic.Model.Category
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
        deriving Eq Show Typeable

    Team
        name Text
        game Game Maybe
        UniqueTeamName name

        gameTitle Text Maybe MigrationOnly

        -- Kept for safer migration
        gameId GameId Maybe MigrationOnly

        deriving Eq Show

    Game
        title Text default ""
        screenshotUrl Text Maybe
        UniqueGameTitle title
        deriving Eq Show

    VotingBucket
        name  Text
        round Int
        deriving Eq Show

    VotingBucketTeam
        team   TeamId
        bucket VotingBucketId

    VotingBucketGame
        team   TeamId
        bucket VotingBucketId

    Vote
        owner    UserId
        value    Int
        game     TeamId
        bucket   VotingBucketId
        category Category

        UniqueVote owner bucket game category
        deriving Eq Show

    -- There should be at most one row in this table.
    CurrentRound
        -- Global current voting round
        round Int
        UniqueCurrentRound
        deriving Eq Show
|]

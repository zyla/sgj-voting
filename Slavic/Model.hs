module Slavic.Model (
    module Slavic.Model
  , module Database.Persist.Sql
) where

import ClassyPrelude
import Database.Persist.Sql
import Database.Persist.TH
import Slavic.Model.Category

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
        team      Team Maybe

        UniqueUserToken token
        UniqueUserNick nick
        deriving Show
        deriving Eq

    Team
        name Text
        game Game Maybe
        UniqueTeamName name
        deriving Show
        deriving Eq

    Game
        title  Text
--        screenshot Text
        UniqueGameTitle title
        deriving Show
        deriving Eq

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

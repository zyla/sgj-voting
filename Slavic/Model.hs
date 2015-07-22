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

        UniqueUserToken token
        UniqueUserNick nick
        deriving Show

    Team
        name       Text
        gameTitle  Text
--        screenshot Text
        UniqueTeamName name
        UniqueTeamGameTitle gameTitle

    TeamMember
        team Team
        user User

    VotingBucket
        round Int

    VotingBucketTeam
        team   Team
        bucket VotingBucket

    Vote
        owner    User
        value    Int
        game     Team
        bucket   VotingBucket
        category Category
|]

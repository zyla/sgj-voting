module Slavic.Model.VotingSpec (spec) where

import TestImport
import Slavic.Model
import Slavic.Model.Voting
import qualified Database.Persist.Sql as Persist

createTestUserWithTeam = do
    user <- createTestUser
    teamId <- insert $ Team "team" $ Just $ Game "game" Nothing
    update (entityKey user) [UserTeam =. Just teamId]
    return (Entity (entityKey user) (entityVal user) { userTeam = Just teamId }, teamId)

mkGame name = Game name Nothing

spec :: Spec
spec = withApp $ do
    describe "getCurrentRound" $ do
        context "when there's no round" $ it "should return Nothing" $ do
            round <- runDB getCurrentRound
            liftIO $ round `shouldBe` Nothing

        context "when there's a round" $ it "should return its number" $ do
            runDB $ insert_ $ CurrentRound 5
            round <- runDB getCurrentRound
            liftIO $ round `shouldBe` Just 5

    describe "vote" $ do
        context "when there's no current round" $ it "should return NoCurrentRound" $ do
            result <- runDB $ do
                user <- createTestUser
                otherTeamId <- insert $ Team "other team" Nothing
                vote user otherTeamId Graphics 5

            liftIO $ result `shouldBe` Left NoCurrentRound

        context "when there's no bucket for this user" $ it "should return NoBucket" $ do
            result <- runDB $ do
                insert_ $ CurrentRound 1
                (user, teamId) <- createTestUserWithTeam
                gameTeamId <- insert $ Team "team1" $ Just $ mkGame "game for voting"

                -- same round, but other team
                otherTeamId <- insert $ Team "other team" Nothing
                bucketId <- insert $ VotingBucket "test" 1
                insert_ $ VotingBucketTeam otherTeamId bucketId

                -- different round
                diffRoundBucketId <- insert $ VotingBucket "test2" 2
                insert_ $ VotingBucketTeam teamId diffRoundBucketId

                vote user gameTeamId Graphics 5

            liftIO $ result `shouldBe` Left NoBucket

        context "when game is in another bucket" $ it "should return NoBucket" $ do
            result <- runDB $ do
                insert_ $ CurrentRound 1
                (user, teamId) <- createTestUserWithTeam
                gameTeamId <- insert $ Team "team1" $ Just $ mkGame "game for voting"

                bucketId <- insert $ VotingBucket "test" 1
                insert_ $ VotingBucketTeam teamId bucketId

                otherBucketId <- insert $ VotingBucket "test2" 1
                insert_ $ VotingBucketGame gameTeamId otherBucketId

                vote user gameTeamId Graphics 5

            liftIO $ result `shouldBe` Left NoBucket

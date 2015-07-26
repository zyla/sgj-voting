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

dummyCategory = Innowacyjnosc

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
                vote user otherTeamId dummyCategory 5

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

                vote user gameTeamId dummyCategory 5

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

                vote user gameTeamId dummyCategory 5

            liftIO $ result `shouldBe` Left NoBucket

        context "when there's no vote" $ it "should create vote" $ do
            (result, gameTeamId, userId) <- runDB $ do
                insert_ $ CurrentRound 1
                (user, teamId) <- createTestUserWithTeam
                gameTeamId <- insert $ Team "team1" $ Just $ mkGame "game for voting"

                bucketId <- insert $ VotingBucket "test" 1
                insert_ $ VotingBucketTeam teamId bucketId
                insert_ $ VotingBucketGame gameTeamId bucketId

                result <- vote user gameTeamId dummyCategory 5
                return (result, gameTeamId, entityKey user)

            Entity voteId Vote{..} <- assertRight result
            liftIO $ do
                voteCategory `shouldBe` dummyCategory
                voteValue `shouldBe` 5
                voteGame `shouldBe` gameTeamId
                voteOwner `shouldBe` userId

        context "when there's a vote" $ it "should update vote" $ do
            (result, gameTeamId, userId, oldVoteId) <- runDB $ do
                insert_ $ CurrentRound 1
                (user, teamId) <- createTestUserWithTeam
                gameTeamId <- insert $ Team "team1" $ Just $ mkGame "game for voting"

                bucketId <- insert $ VotingBucket "test" 1
                insert_ $ VotingBucketTeam teamId bucketId
                insert_ $ VotingBucketGame gameTeamId bucketId

                oldVoteId <- insert $ Vote (entityKey user) 1 gameTeamId bucketId dummyCategory 

                result <- vote user gameTeamId dummyCategory 5
                return (result, gameTeamId, entityKey user, oldVoteId)

            Entity voteId _ <- assertRight result
            Just Vote{..} <- runDB $ Persist.get voteId
            liftIO $ do
                voteId `shouldBe` oldVoteId
                voteCategory `shouldBe` dummyCategory
                voteValue `shouldBe` 5
                voteGame `shouldBe` gameTeamId
                voteOwner `shouldBe` userId

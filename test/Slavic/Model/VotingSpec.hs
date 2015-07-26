module Slavic.Model.VotingSpec (spec) where

import TestImport
import Slavic.Model
import Slavic.Model.Voting

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

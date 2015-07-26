module Slavic.Model.TeamSpec (spec) where

import TestImport
import Slavic.Model hiding (get)
import Slavic.Model.User (makeUser)
import Slavic.Model.Team

spec :: Spec
spec = withApp $ do
    describe "addUserToTeam" $ do
        it "works" $ do
            let team = Team "test team" Nothing
            userE@(Entity userId user) <- runDB $ createUserWithCreds "jdoe" "lambdacard"
            teamId <- runDB $ insert team

            liftIO $ userTeam user `shouldBe` Nothing

            runDB $ addUserToTeam teamId userId

            Just (Entity _ user) <- runDB $ selectFirst [] []

            liftIO $ userTeam user `shouldBe` Just teamId

    describe "removeUserFromTeam" $ do
        it "removes user from team" $ do
            let team = Team "test team" Nothing
            userE@(Entity userId user) <- runDB $ createUserWithCreds "jdoe" "lambdacard"
            teamId <- runDB $ insert team
            runDB $ update userId [UserTeam =. Just teamId]
            Just (Entity userId user) <- runDB $ selectFirst [] []
            liftIO $ userTeam user `shouldBe` Just teamId

            runDB $ removeUserFromTeam userId

            Just (Entity _ user) <- runDB $ selectFirst [] []

            liftIO $ userTeam user `shouldBe` Nothing

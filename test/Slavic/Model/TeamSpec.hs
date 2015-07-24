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
            _ <- runDB $ insert team

            liftIO $ userTeam user `shouldBe` Nothing

            runDB $ addUserToTeam team userId

            Just (Entity _ user) <- runDB $ selectFirst [] []

            liftIO $ userTeam user `shouldBe` Just team

createUserWithCreds login password = do
    tokenId <- insert $ Token "123123"
    insertEntity =<< liftIO (makeUser tokenId login password "John" "Doe" "Warsaw")

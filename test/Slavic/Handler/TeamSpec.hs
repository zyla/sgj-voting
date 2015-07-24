module Slavic.Handler.TeamSpec (spec) where

import TestImport
import Slavic.Handler.Team
import Slavic.Model hiding (get)
import Slavic.Model.User (makeUser)

createUserWithCreds login password = do
    tokenId <- insert $ Token "123123"
    insertEntity =<< liftIO (makeUser tokenId login password "John" "Doe" "Warsaw")


spec :: Spec
spec = withApp $ do
    describe "addTeam" $ do
        context "when user is not logged in" $ --also tests withAuthUser
            it "should redirect to login" $ do
                get AddTeamR
                statusIs 303
                assertHeader "Location" "/login"

        context "when user is in a team" $
            it "should redirect to root" $ do
                Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                Entity _ team <- runDB $ insertEntity $ Team "test" Nothing
                runDB $ update userId [UserTeam =. Just team]
                loginWith "jdoe" "lambdacard"

                get AddTeamR
                statusIs 303
                assertHeader "Location" "/"

        context "when user has no team" $ do
            it "should display form" $ do
                Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                loginWith "jdoe" "lambdacard"

                get AddTeamR
                statusIs 200
                htmlCount "input[type=text][name=name]" 1

            context "when user creates new team" $ do
                it "should create team and add creator to the team" $ do
                    Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                    loginWith "jdoe" "lambdacard"

                    get AddTeamR
                    statusIs 200

                    request $ do
                       setMethod "POST"
                       setUrl AddTeamR
                       addCSRFToken
                       addPostParam "name" "test team"
                    statusIs 303

                    Just (Entity _ team) <- runDB $ selectFirst [] []
                    liftIO $ teamName team `shouldBe` "test team"

                    Just (Entity _ user) <- runDB $ selectFirst [UserId ==. userId] []
                    let Just uTeam = userTeam user
                    liftIO $ teamName uTeam `shouldBe` "test team"

    describe "leaveTeam" $ do
        context "when user is not logged in" $ --also tests withAuthUser
            it "should redirect to login" $ do
                get LeaveTeamR
                statusIs 303
                assertHeader "Location" "/login"

        context "when user is in not in any team" $
            it "should redirect to root" $ do
                Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                loginWith "jdoe" "lambdacard"

                get LeaveTeamR
                statusIs 303
                assertHeader "Location" "/"

        context "when user is in a team" $
            it "should remove them from team" $ do
                Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                Entity _ team <- runDB $ insertEntity $ Team "test" Nothing
                runDB $ update userId [UserTeam =. Just team]
                loginWith "jdoe" "lambdacard"

                request $ do
                   setMethod "POST"
                   setUrl LeaveTeamR
                   addPostParam "confirm" "true"
                statusIs 303

                Just (Entity _ user) <- runDB $ selectFirst [UserId ==. userId] []
                liftIO $ userTeam user `shouldBe` Nothing

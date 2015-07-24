module Slavic.Handler.TeamSpec (spec) where

import TestImport
import Slavic.Handler.Team
import Slavic.Model hiding (get)
import Slavic.Model.User (makeUser)

import qualified Database.Persist.Sql as Persist

createUserWithCreds login password = do
    tokenId <- insert $ Token "123123"
    insertEntity =<< liftIO (makeUser tokenId login password "John" "Doe" "Warsaw")

createUserAndLogin login password = do
    user <- runDB $ createUserWithCreds login password
    loginWith login password
    statusIs 303
    return user

makeTeam :: Int64 -> Text -> SqlPersistM (Entity Team)
makeTeam id name =
    let entity = Entity (toSqlKey id) $ Team name Nothing
    in insertKey (entityKey entity) (entityVal entity) >> return entity

makeTeamWithMembers :: Int64 -> Text -> [Text] -> SqlPersistM (Entity Team)
makeTeamWithMembers id name members = do
    let teamId = toSqlKey id
        entity = Entity teamId $ Team name Nothing
    insertKey teamId (entityVal entity)

    forM_ members $ \nick -> do
        tokenId <- insert $ Token nick
        insert_ $ User tokenId nick "" nick nick nick (Just teamId)

    return entity

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
                Entity teamId team <- runDB $ insertEntity $ Team "test" Nothing
                runDB $ update userId [UserTeam =. Just teamId]
                loginWith "jdoe" "lambdacard"

                get AddTeamR
                statusIs 303
                assertHeader "Location" "/"

        context "when user has no team" $ do
            it "should display form" $ do
                createUserAndLogin "jdoe" "lambdacard"

                get AddTeamR
                statusIs 200
                htmlCount "input[type=text][name=name]" 1

            context "when user creates new team" $ do
                it "should create team and add creator to the team" $ do
                    Entity userId user <- createUserAndLogin "jdoe" "lambdacard"

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
                    let Just uTeamId = userTeam user
                    Just uTeam <- runDB $ Persist.get uTeamId
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
                Entity teamId team <- runDB $ insertEntity $ Team "test" Nothing
                runDB $ update userId [UserTeam =. Just teamId]
                loginWith "jdoe" "lambdacard"

                request $ do
                   setMethod "POST"
                   setUrl LeaveTeamR
                   addPostParam "confirm" "true"
                statusIs 303

                Just (Entity _ user) <- runDB $ selectFirst [UserId ==. userId] []
                liftIO $ userTeam user `shouldBe` Nothing

    describe "root handler" $ context "when user is logged in" $ do
        it "should display teams with members" $ do
            createUserAndLogin "jdoe" "lambdacard"

            teams <- runDB $ sequence
                [ makeTeamWithMembers 1 "Monadic Warriors" ["zyla", "buoto", "KrzyStar"]
                , makeTeamWithMembers 2 "Haskell Bank" ["przembot"]
                , makeTeamWithMembers 3 "Zygohistoprepromorphisms" []
                ]

            get RootR
            statusIs 200

            htmlAnyContain "tr" "<td>Haskell Bank</td>\n<td>przembot</td>"
            htmlAnyContain "tr" "<td>Monadic Warriors</td>\n<td>zyla, buoto, KrzyStar</td>"
            htmlAnyContain "tr" "<td>Zygohistoprepromorphisms</td>\n<td></td>"

        it "should display create team link" $ do
            createUserAndLogin "jdoe" "lambdacard"

            get RootR
            statusIs 200

            htmlAnyContain "a" "Create new team"

        it "should display create team link" $ do
            userId  <- entityKey <$> createUserAndLogin "jdoe" "lambdacard"
            team <- runDB $ makeTeam 1 "Monadic Warriors"
            runDB $ update userId [ UserTeam =. Just (entityKey team) ]

            get RootR
            statusIs 200

            htmlAnyContain "p" "You are now in team Monadic Warriors"

    describe "joinTeam" $ do
        context "when user is in a team" $
            it "should redirect to root" $ do
                Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                Entity teamId team <- runDB $ insertEntity $ Team "test" Nothing
                runDB $ update userId [UserTeam =. Just teamId]
                loginWith "jdoe" "lambdacard"

                Entity newTeamId _ <- runDB $ insertEntity $ Team "new team" Nothing

                post $ JoinTeamR newTeamId
                statusIs 303
                assertHeader "Location" "/"

        context "when user is in not in any team" $
            it "should join the team" $ do
                Entity userId user <- runDB $ createUserWithCreds "jdoe" "lambdacard"
                loginWith "jdoe" "lambdacard"

                Entity newTeamId _ <- runDB $ insertEntity $ Team "new team" Nothing

                post $ JoinTeamR newTeamId
                statusIs 303

                Just (Entity _ user) <- runDB $ selectFirst [UserId ==. userId] []
                liftIO $ userTeam user `shouldBe` Just newTeamId

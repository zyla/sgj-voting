module Slavic.Handler.TeamSpec (spec) where

import TestImport
import Slavic.Handler.Team
import Slavic.Model hiding (get)
import Slavic.Model.User (makeUser, makePassword)

import qualified Database.Persist.Sql as Persist

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
        pass <- liftIO $ makePassword nick
        insert_ $ User tokenId nick pass nick nick nick (Just teamId)

    return entity

addTestTeams = runDB $ sequence
    [ makeTeamWithMembers 1 "Monadic Warriors" ["zyla", "buoto", "KrzyStar"]
    , makeTeamWithMembers 2 "Haskell Bank" ["przembot"]
    , makeTeamWithMembers 3 "Zygohistoprepromorphisms" []
    ]

monadicWarriorsId = toSqlKey 1

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

            htmlAnyContain "p" "<span>You are now in team</span>\n<a href=\"/teams/1\">Monadic Warriors</a>"

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

    describe "teamView" $ do
        let assertTestTeamDisplayed = do
                htmlAllContain "title" "Team Monadic Warriors - Slavic Game Jam"

                htmlAllContain "h2" "Team Monadic Warriors"
                htmlAnyContain "p" "Members: zyla, buoto, KrzyStar"

        context "when user is not logged in" $ it "should redirect to login" $ do
            addTestTeams
            assertRedirectsToLogin $ TeamR $ toSqlKey 1

        context "when team has no game" $ it "should display only team name and members" $ do
            createUserAndLogin "jdoe" "lambdacard"
            addTestTeams

            get $ TeamR monadicWarriorsId
            statusIs 200

            assertTestTeamDisplayed

        context "when team has game" $ it "should display game name and screenshot" $ do
            createUserAndLogin "jdoe" "lambdacard"
            addTestTeams

            runDB $ update monadicWarriorsId
                [TeamGame =. Just (Game "Cool game" $ Just "http://zyla.neutrino.re/share/waves.png")]

            get $ TeamR monadicWarriorsId
            statusIs 200
            assertTestTeamDisplayed

            htmlAnyContain "p" "Game: Cool game"
            htmlCount "img.screenshot[src=http://zyla.neutrino.re/share/waves.png]" 1

        context "when user views other team" $ it "should not display edit button" $ do
            addTestTeams
            createUserAndLogin "jdoe" "lambdacard"
            get $ TeamR monadicWarriorsId
            statusIs 200

            htmlNoneContain "a" "Edit game information"

        context "when user views his team" $ it "should display edit button" $ do
            addTestTeams
            loginWith "zyla" "zyla"
            get $ TeamR monadicWarriorsId
            statusIs 200

            htmlAnyContain "a[href=/edit_game]" "Edit game information"

    describe "editGame" $ do
        context "when user is not logged in" $ it "should redirect to login" $ do
            addTestTeams
            assertRedirectsToLogin EditGameR

        context "when user has no team" $ it "should redirect to root" $ do
            createUserAndLogin "jdoe" "lambdacard"

            get EditGameR
            statusIs 303
            assertHeader "Location" "/"

        it "should display form" $ do
            addTestTeams
            loginWith "zyla" "zyla"

            get EditGameR
            statusIs 200
            htmlAllContain "title" "Edit game information - SGJ"
            htmlCount "input[type=text][name=title]" 1
            htmlCount "input[type=text][name=screenshot_url]" 1
            htmlCount "input[type=submit][value=Save]" 1

        context "when team has game" $ it "should display form with filled values" $ do
            addTestTeams
            loginWith "zyla" "zyla"

            runDB $ update monadicWarriorsId [TeamGame =. Just (Game "Cool game" Nothing)]

            get EditGameR
            statusIs 200

            htmlAllContain "title" "Edit game information - SGJ"
            htmlCount "input[type=text][name=title][value=Cool game]" 1
            htmlCount "input[type=submit][value=Save]" 1

        it "should update game" $ do
            addTestTeams
            loginWith "zyla" "zyla"

            get EditGameR

            request $ do
               setMethod "POST"
               setUrl EditGameR
               addCSRFToken
               addPostParam "title" "test game"
            statusIs 303
            assertHeader "Location" "/teams/1"

            team <- runDB $ Persist.get monadicWarriorsId
            liftIO $ (team >>= teamGame) `shouldBe` Just (Game "test game" Nothing)

        it "should update game with screenshot" $ do
            addTestTeams
            loginWith "zyla" "zyla"

            get EditGameR

            request $ do
               setMethod "POST"
               setUrl EditGameR
               addCSRFToken
               addPostParam "title" "test game"
               addPostParam "screenshot_url" "http://zyla.neutrino.re/share/waves.png"
            statusIs 303
            assertHeader "Location" "/teams/1"

            team <- runDB $ Persist.get monadicWarriorsId
            liftIO $ (team >>= teamGame) `shouldBe`
                Just (Game "test game" $ Just "http://zyla.neutrino.re/share/waves.png")

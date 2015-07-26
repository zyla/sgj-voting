{-# LANGUAGE TupleSections #-}
module Slavic.Handler.Team where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User
import Slavic.Model.Team (TeamWithMembers(..), addUserToTeam, removeUserFromTeam, getTeams)
import Slavic.Forms
import Slavic.Handler.Util
import Text.Blaze (text)

makeTeamForm :: Html -> MForm Handler (FormResult Team, Widget)
makeTeamForm = renderTable $ Team
    <$> areq textField (mkFieldSettings "Team name" "name") Nothing
    <*> pure Nothing


getAddTeamR :: Handler Html
getAddTeamR = withAuthUser $ \(Entity _userId user) -> do
    case userTeam user of
        Just _team -> redirect RootR
        Nothing -> do
            (widget, enctype) <- generateFormPost makeTeamForm
            displayAddTeamForm widget enctype

postAddTeamR :: Handler Html
postAddTeamR = withAuthUser $ \(Entity userId user) -> do
    case userTeam user of
        Just _team -> redirect RootR
        Nothing -> do
            ((result,widget), enctype) <- runFormPost makeTeamForm
            case result of
                FormSuccess team -> do
                    teamId <- addTeam team
                    runDB $ addUserToTeam teamId userId
                    redirect RootR
                FormFailure _errors -> displayAddTeamForm widget enctype
                FormMissing -> displayAddTeamForm widget enctype
    where
        addTeam = runDB . insert

displayAddTeamForm :: Widget -> Enctype -> Handler Html
displayAddTeamForm widget enctype =
    defaultLayout $ do
    setTitle "Add team - SGJ"
    [whamlet|
        <form method=post action=@{AddTeamR} enctype=#{enctype}>
            <table>^{widget}
            <input type=submit value="Submit">
        |]

getLeaveTeamR :: Handler Html
getLeaveTeamR = withAuthUser $ \(Entity _userId user) -> do
    case userTeam user of
        Nothing    -> redirect RootR
        Just teamId -> do
            -- FIXME squash the monads
            runDB (get teamId) >>= \case
                Nothing -> redirect RootR
                Just Team{teamName=team} -> defaultLayout $ do
                    setTitle $ "Leave team " ++ text team ++ " - SGJ"
                    [whamlet|
                        <p>Do you really want to leave team #{team}
                        <form method=post action=@{LeaveTeamR}>
                            <input type=submit value="Leave team">
                        |]

postLeaveTeamR :: Handler Html
postLeaveTeamR = withAuthUser $ \(Entity userId user) -> do
    case userTeam user of
        Nothing    -> redirect RootR
        Just _team -> do
            runDB $ removeUserFromTeam userId
            redirect RootR

-- | Used in root handler to display team list page when user is logged in.
displayTeams :: Handler Html
displayTeams = withAuthUser $ \(Entity _ authUser) -> do
    teams <- runDB getTeams
    currentTeam <- case userTeam authUser of
        Nothing -> return Nothing
        Just teamId -> runDB $ fmap (Entity teamId) <$> get teamId
    defaultLayout $ do
        setTitle "Teams - Slavic Game Jam"
        $(whamletFile "templates/current_team.hamlet")
        $(whamletFile "templates/teams.hamlet")

postJoinTeamR :: TeamId -> Handler Html
postJoinTeamR teamId = withAuthUser $ \(Entity userId user) -> do
    case userTeam user of
        Nothing -> do
            runDB $ addUserToTeam teamId userId
            redirect RootR
        Just _team -> redirect RootR

getTeamR :: TeamId -> Handler Html
getTeamR teamId = do
    Entity _ user <- requireAuth
    team <- fromMaybeOrNotFound =<< runDB (get teamId)
    members <- runDB $ selectList [UserTeam ==. Just teamId] [Asc UserId]
    let maybeGame = teamGame team
    let ownTeam = userTeam user == Just teamId
    defaultLayout $ do
        setTitle $ "Team " ++ text (teamName team) ++ " - Slavic Game Jam"
        $(whamletFile "templates/team.hamlet")

fromMaybeOrNotFound :: Maybe a -> Handler a
fromMaybeOrNotFound Nothing = notFound
fromMaybeOrNotFound (Just a) = pure a

requireUserWithTeam :: Handler (Entity User, Entity Team)
requireUserWithTeam = do
    user <- requireAuth
    team <- runDB $ map join $ traverse getEntity $ userTeam $ entityVal user
    maybe (redirect RootR) (return . (user,)) team
  where
    getEntity key = map (Entity key) <$> get key

getEditGameR :: Handler Html
getEditGameR = do
    (_, Entity _ team) <- requireUserWithTeam
    (widget, enctype) <- generateFormPost (editGameForm $ teamGame team)
    displayEditGameForm widget enctype

postEditGameR :: Handler Html
postEditGameR = do
    (_, Entity teamId team) <- requireUserWithTeam
    ((result, widget), enctype) <- runFormPost (editGameForm $ teamGame team)
    case result of
        FormSuccess game -> do
            runDB $ update teamId [TeamGame =. Just game]
            redirect $ TeamR teamId
        _ -> displayAddTeamForm widget enctype

displayEditGameForm :: Widget -> Enctype -> Handler Html
displayEditGameForm widget enctype =
    defaultLayout $ do
        setTitle "Edit game information - SGJ"
        [whamlet|
            <form method=post enctype=#{enctype}>
                <table>^{widget}
                <input type=submit value="Save">
            |]

editGameForm :: Maybe Game -> Html -> MForm Handler (FormResult Game, Widget)
editGameForm game = renderTable $ Game
    <$> areq textField (mkFieldSettings "Game title" "title") (gameTitle <$> game)

module Slavic.Handler.Team where

import ClassyPrelude.Slavic
import Yesod
import Slavic.Foundation
import Slavic.Model
import Slavic.Model.User
import Slavic.Model.Team (addUserToTeam, removeUserFromTeam, getTeams)
import Slavic.Handler.Util
import Text.Blaze (text)

makeTeamForm :: Html -> MForm Handler (FormResult Team, Widget)
makeTeamForm = renderTable $ Team
    <$> areq textField (fs "Team name" "name") Nothing
    <*> pure Nothing
  where
    fs label name = (fieldSettingsFromLabel label) { fsName = Just name, fsId = Just name }
    fieldSettingsFromLabel = fromString


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
                    _ <- addTeam team
                    runDB $ addUserToTeam team userId
                    redirect AddTeamSuccessfulR
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


getAddTeamSuccessfulR :: Handler Html
getAddTeamSuccessfulR = withAuthUser $ \(Entity _ user) ->
    let team = userTeam user in
    defaultLayout $ do
        setTitle "Team created"
        $(whamletFile "templates/changeteam_successful.hamlet")

getLeaveTeamR :: Handler Html
getLeaveTeamR = withAuthUser $ \(Entity _userId user) -> do
    case userTeam user of
        Nothing    -> redirect RootR
        Just team_ -> do
            let team = teamName team_
            defaultLayout $ do
                setTitle ("Leave team " ++ text team ++ " - SGJ")
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
            redirect LeaveTeamSuccessfulR

getLeaveTeamSuccessfulR :: Handler Html
getLeaveTeamSuccessfulR = withAuthUser $ \(Entity _ user) ->
    let team = userTeam user in
    defaultLayout $ do
        setTitle "Left the team"
        $(whamletFile "templates/changeteam_successful.hamlet")

-- | Used in root handler to display team list page when user is logged in.
displayTeams :: Handler Html
displayTeams = do
    teams <- runDB getTeams
    defaultLayout $ do
        setTitle "Teams - Slavic Game Jam"
        $(whamletFile "templates/teams.hamlet")

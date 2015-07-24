module Slavic.Handler.LoginSpec (spec) where

import TestImport
import Slavic.Handler.Login
import Slavic.Model hiding (get)
import Slavic.Model.User

spec :: Spec
spec = withApp $ do
    describe "login handler" $ do
        it "should display form" $ do
            get LoginR
            statusIs 200
            htmlCount "input[type=text][name=nick]" 1
            htmlCount "input[type=password][name=password]" 1

        let createUserWithCreds login password = do
                tokenId <- insert $ Token "123123"
                insertEntity =<< liftIO (makeUser tokenId login password "John" "Doe" "Warsaw")

        context "with correct data" $ it "should log in" $ do
            user <- runDB $ createUserWithCreds "jdoe" "lambdacard"

            loginWith "jdoe" "lambdacard"
            statusIs 303
            
            get RootR
            statusIs 200
            -- This tests also the root handler, but it's the easiest way to
            -- check if we're logged in.
            bodyContains "jdoe"

        context "with invalid nick" $ it "should display error" $ do
            user <- runDB $ createUserWithCreds "jdoe" "lambdacard"

            loginWith "bad_nick" "lambdacard"
            statusIs 200
            htmlAllContain ".errors" "Invalid nick/password"
            
            get RootR
            statusIs 200
            assertBodyDoesntContain "jdoe"

        context "with invalid password" $ it "should display error" $ do
            user <- runDB $ createUserWithCreds "jdoe" "lambdacard"

            loginWith "jdoe" "bad_password"
            statusIs 200
            htmlAllContain ".errors" "Invalid nick/password"
            
            get RootR
            statusIs 200
            assertBodyDoesntContain "jdoe"

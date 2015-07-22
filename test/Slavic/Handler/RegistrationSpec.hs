module Slavic.Handler.RegistrationSpec (spec) where

import TestImport
import Slavic.Handler.Registration
import Slavic.Model hiding (get)

spec :: Spec
spec = withApp $ describe "registration" $ do
    it "should display form" $ do
        get RegisterR
        statusIs 200
        htmlCount "input[type=text][name=token]" 1
        htmlCount "input[type=text][name=nick]" 1
        htmlCount "input[type=text][name=first_name]" 1
        htmlCount "input[type=text][name=last_name]" 1
        htmlCount "input[type=text][name=city]" 1
        htmlCount "input[type=password][name=password]" 1

    context "with correct data" $ it "should register user" $ do
        let dummyToken = "123123"
        tokenId <- runDB $ insert $ Token dummyToken

        get RegisterR
        statusIs 200

        request $ do
           setMethod "POST"
           setUrl RegisterR
           addCSRFToken
           addPostParam "token" dummyToken
           addPostParam "nick" "jdoe"
           addPostParam "first_name" "John"
           addPostParam "last_name" "Doe"
           addPostParam "city" "Warsaw"
           addPostParam "password" "lambdacard"
        statusIs 303

        Just (Entity _ User{..}) <- runDB $ getBy $ UniqueUserToken tokenId
        liftIO $ do
            userNick `shouldBe` "jdoe"
            userFirstName `shouldBe` "John"
            userLastName `shouldBe` "Doe"
            userCity `shouldBe` "Warsaw"

            -- FIXME move to enscripp'd passwords!
            userPassword `shouldBe` "lambdacard"

    context "with nonexistent token" $ it "should return error" $ do
        let dummyToken = "this token does not exist"
        runDB $ insert_ $ Token "other token"

        get RegisterR
        statusIs 200

        request $ do
           setMethod "POST"
           setUrl RegisterR
           addCSRFToken
           addPostParam "token" dummyToken
           addPostParam "nick" "jdoe"
           addPostParam "first_name" "John"
           addPostParam "last_name" "Doe"
           addPostParam "city" "Warsaw"
           addPostParam "password" "lambdacard"
        statusIs 200 -- FIXME find a way to make Yesod return different code for errors

        htmlAllContain ".errors" "Invalid token"

    context "with already registered token" $ it "should return error" $ do
        let dummyToken = "123123"
        runDB $ do
            tokenId <- insert $ Token dummyToken
            insert_ $ User tokenId "jdoe" "lambdacard" "John" "Doe" "Warsaw"

        get RegisterR
        statusIs 200

        request $ do
           setMethod "POST"
           setUrl RegisterR
           addCSRFToken
           addPostParam "token" dummyToken
           addPostParam "nick" "test"
           addPostParam "first_name" "Test"
           addPostParam "last_name" "Test"
           addPostParam "city" "Warsaw"
           addPostParam "password" "password"
        statusIs 200 -- FIXME find a way to make Yesod return different code for errors

        htmlAllContain ".errors" "Token already registered"

    -- TODO add tests for missing fields

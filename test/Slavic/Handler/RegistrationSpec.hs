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
        runDB $ insert_ $ Token dummyToken

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

        Just (Entity _ User{..}) <- runDB $ getBy $ UniqueUserToken dummyToken
        liftIO $ do
            userNick `shouldBe` "jdoe"
            userFirstName `shouldBe` "John"
            userLastName `shouldBe` "Doe"
            userCity `shouldBe` "Warsaw"

            -- FIXME move to enscripp'd passwords!
            userPassword `shouldBe` "lambdacard"

    -- TODO add tests for incorrect input (bad token, missing fields)

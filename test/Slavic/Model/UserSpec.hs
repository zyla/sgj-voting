module Slavic.Model.UserSpec (spec) where

import TestImport
import Slavic.Model hiding (get)
import Slavic.Model.User

spec :: Spec
spec = withApp $ do
    describe "login" $ do
        context "with correct credentials" $ it "should return user" $ do
            user <- runDB $ createUserWithCreds "jdoe" "lambdacard"

            result <- runDB $ login "jdoe" "lambdacard"
            liftIO $ result `shouldBe` Right user

        context "with bad nick" $ it "should return error" $ do
            user <- runDB $ createUserWithCreds "jdoe" "lambdacard"

            result <- runDB $ login "GUWNO" "lambdacard"
            liftIO $ result `shouldBe` Left "Invalid credentials"

        context "with bad password" $ it "should return error" $ do
            user <- runDB $ createUserWithCreds "jdoe" "lambdacard"

            result <- runDB $ login "jdoe" "badpassword"
            liftIO $ result `shouldBe` Left "Invalid credentials"

createUserWithCreds login password = do
    tokenId <- insert $ Token "123123"
    insertEntity =<< liftIO (makeUser tokenId login password "John" "Doe" "Warsaw")

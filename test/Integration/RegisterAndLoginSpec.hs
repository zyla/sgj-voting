module Integration.RegisterAndLoginSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
    it "should register user and log in" $ do
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

        resetCookies

        loginWith "jdoe" "lambdacard"
        statusIs 303

        get RootR
        statusIs 200
        bodyContains "jdoe"

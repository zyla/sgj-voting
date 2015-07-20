{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Yesod
import Network.Wai.Handler.Warp (run)

data Slavic = Slavic

mkYesod "Slavic" [parseRoutes|
    / RootR GET
|]

instance Yesod Slavic where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

getRootR :: Handler Html
getRootR = defaultLayout $ do
    setTitle "My title"
    [whamlet|<p>Hello world|]

main :: IO ()
main = run 3000 =<< toWaiApp Slavic

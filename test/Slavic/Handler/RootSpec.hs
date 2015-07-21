module Slavic.Handler.RootSpec (spec) where

import TestImport
import Slavic.Handler.Root

spec :: Spec
spec = withApp $ describe "root handler" $ do
    it "works" $ do
        get RootR
        statusIs 200
        htmlAllContain "title" "Slavic Game Jam"

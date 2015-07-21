module Main where

import ClassyPrelude
import Slavic
import Yesod (toWaiApp)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = makeApp >>= toWaiApp >>= Warp.run 3000

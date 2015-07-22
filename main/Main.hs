module Main where

import ClassyPrelude
import Slavic
import Yesod (toWaiApp)
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Logger (runStdoutLoggingT)

main :: IO ()
main = runStdoutLoggingT (makeApp "dbname=slavic") >>= toWaiApp >>= Warp.run 3000

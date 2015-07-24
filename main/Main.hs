module Main where

import ClassyPrelude
import Slavic
import Yesod (toWaiApp)
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Logger (runStdoutLoggingT)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    -- TODO parameterize also PGUSER, PGHOST etc
    dbname <- fromString <$> fromMaybe "slavic" <$> lookupEnv "PGDATABASE"
    let connectionString = "dbname=" ++ dbname
    runStdoutLoggingT (makeApp connectionString) >>= toWaiApp >>= Warp.run 3000

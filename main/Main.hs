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
    port <- fromMaybe 3000 <$> (>>= readMay) <$> lookupEnv "PORT"
    let connectionString = "dbname=" ++ dbname
    putStrLn $ "Running on port " ++ tshow port
    runStdoutLoggingT (makeApp connectionString) >>= toWaiApp >>= Warp.run port

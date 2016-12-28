{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Pool (createPool)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import Network.Wai.Handler.Warp
import qualified Database.PostgreSQL.Simple as PG

import ChannelStatus.Server


main :: IO ()
main = do
    let port = 8081
    T.putStrLn $ "Serving on http://localhost:" <> (pack . show $ port)
    pool <- createPool (PG.connectPostgreSQL "postgresql:///channelstatus") PG.close 10 5 10
    run port (app pool)

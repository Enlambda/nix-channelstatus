{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module ChannelStatus.Server where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Servant
import Data.Pool (Pool, withResource)
import Data.Text
import Database.PostgreSQL.Simple hiding (Query)

import ChannelStatus.API
import ChannelStatus.DatabaseExtra
import ChannelStatus.Query


filesapi :: Maybe Text -> AppM [(Storepathcontent, Storepath)]
filesapi q = do
  conn <- getConn
  liftIO $ searchBinaries conn q

channelapi :: AppM [Channel]
channelapi = return [Channel "foo", Channel "bar"]

server :: ServerT API AppM
server = filesapi
--  :<|> channelapi

channelsAPI :: Proxy API
channelsAPI = Proxy

type AppM = ReaderT (Pool Connection) (ExceptT ServantErr IO)

readerTToExcept :: Pool Connection -> AppM :~> ExceptT ServantErr IO
readerTToExcept pool = Nat (\r -> runReaderT r pool)

app :: Pool Connection -> Application
app pool = serve channelsAPI $ enter (readerTToExcept pool) server

getConnFromPool :: Pool Connection -> AppM Connection
getConnFromPool pool = withResource pool return

getConn :: AppM Connection
getConn = ask >>= getConnFromPool

{-# LANGUAGE TypeOperators   #-}

module ChannelStatus.Server where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Servant
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple hiding (Query)

import ChannelStatus.API
import ChannelStatus.Query


filesapi :: Maybe String -> AppM [File]
filesapi Nothing = return files
filesapi (Just _) = return files

channelapi :: AppM [Channel]
channelapi = return [Channel "foo", Channel "bar"]

server :: ServerT API AppM
server = filesapi
    :<|> channelapi

channelsAPI :: Proxy API
channelsAPI = Proxy


files :: [File]
files =
    [ File "foo" 3
    , File "bar" 10
    ]


type AppM = ReaderT (Pool Connection) (ExceptT ServantErr IO)

readerTToExcept :: Pool Connection -> AppM :~> ExceptT ServantErr IO
readerTToExcept pool = Nat (\r -> runReaderT r pool)

app :: Pool Connection -> Application
app pool = serve channelsAPI $ enter (readerTToExcept pool) server

getConnFromPool :: Pool Connection -> AppM Connection
getConnFromPool pool = withResource pool return

getConn :: AppM Connection
getConn = ask >>= getConnFromPool

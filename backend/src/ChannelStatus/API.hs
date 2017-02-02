{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module ChannelStatus.API where

import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Servant

import ChannelStatus.Database

type ChannelsAPI = "channel" :>
-- TODO: the tuple needs to become a record
  ( "files" :> QueryParam "q" Text :> Get '[JSON] [(Storepathcontent, Storepath)]
-- :<|> Get '[JSON] [Channel]
  )

data File = File {
  name :: String,
  age :: Int
} deriving (Eq, Show, Generic)

instance ToJSON File

data Channel = Channel {
  cname :: String
} deriving (Eq, Show, Generic)

instance ToJSON Channel

type API = "api" :> ChannelsAPI

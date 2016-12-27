{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ChannelStatus.DatabaseExtra
  ( module ChannelStatus.Database
  ) where

import Data.Aeson
import ChannelStatus.Database


instance ToJSON Storepathcontent where
instance ToJSON Storepath where

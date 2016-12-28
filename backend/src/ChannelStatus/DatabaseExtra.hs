{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ChannelStatus.DatabaseExtra
  ( module ChannelStatus.Database
  ) where

import ChannelStatus.Database
import Data.Aeson
import Servant.Elm


instance ToJSON Storepathcontent where
instance ToJSON Storepath where

instance ElmType Storepathcontent where
instance ElmType Storepath where

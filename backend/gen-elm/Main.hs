{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main
  ( main
  ) where

import Data.Text (Text, replace, pack)
import Elm
import GHC.Int
import Servant.Elm
import Servant.Foreign
import Servant.Foreign.Internal (Elem)
import Options.Applicative

import ChannelStatus.API
import ChannelStatus.DatabaseExtra


elmoptions :: Options
elmoptions = Options {fieldLabelModifier = replace "'" ""}

spec :: ElmOptions -> Spec
spec elmexportoptions = Spec ["API"]
            (defElmImports
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Storepath)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Storepath)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Storepathcontent)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Storepathcontent)
              : generateElmForAPIWith elmexportoptions (Proxy :: Proxy API)
            )

data ElmConfig = ElmConfig
  { elmpath :: String
  , elmbackendurl :: String
  }

parser :: Parser ElmConfig
parser =
      ElmConfig
  <$> argument str (metavar "FOLDER")
  <*> argument str (metavar "BACKEND-URL")

main :: IO ()
main = do
  elmconfig <- execParser $ info (helper <*> parser)
    (fullDesc <> progDesc "Generate types for Elm frontend")
  let elmexportoptions = defElmOptions {
        elmExportOptions = elmoptions
      , urlPrefix = Dynamic
      }
  specsToDir [spec elmexportoptions] $ elmpath elmconfig

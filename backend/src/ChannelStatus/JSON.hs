{-# LANGUAGE OverloadedStrings #-}

module ChannelStatus.JSON
    ( Directory (..)
    , File (..)
    , DirectoryEntry (..)
    , IsExecutable (..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Traversable (for)
import Data.Text (Text)
import Data.Word (Word64)
--import Path (Path)
import qualified Data.HashMap.Strict as HM


type Size = Word64
data IsExecutable = NotExecutable | Executable deriving (Show, Eq)
data File = File Size IsExecutable deriving (Show, Eq)

type Target = Text
type Path = Text

data Directory =
    Directory [DirectoryEntry]
  | FilePath Path File 
  | Symlink Path Target
  deriving (Show, Eq)

data DirectoryEntry = DirectoryEntry Path Directory deriving (Show, Eq)

instance FromJSON Directory where

  parseJSON = withObject "root" $ \o -> do
    -- TODO: assert version is 1 and warn
    root <- o .: "root"
    parseListing "root" root

parseListing :: Text -> Object -> Parser Directory
parseListing key obj =
  case HM.lookup "type" obj of
    (Just "directory") -> do
        entries <- obj .: "entries"
        newpaths <- for (HM.toList entries) $ \(key, value) -> do
          path <- parseListing key value
          return $ DirectoryEntry key path
        return $ Directory newpaths
    (Just "regular") -> do
        size <- (obj .: "size")
        isExecutable <- (obj .:? "executable" .!= NotExecutable)
        return . FilePath key $ File size isExecutable
    (Just "symlink") -> do
        target <- (obj .: "target")
        return $ Symlink key target
    _ -> fail "Unknown type listing"



instance FromJSON IsExecutable where

  parseJSON = withBool "isExecutable" $ \bool ->
    case bool of
      True -> return Executable
      False -> return NotExecutable

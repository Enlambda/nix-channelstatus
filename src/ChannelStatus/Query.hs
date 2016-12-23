{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ChannelStatus.Query where

import           Control.Arrow (returnA)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text, intercalate)
import           Data.Maybe (listToMaybe)
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Int
import           Opaleye

import           ChannelStatus.Database
import           ChannelStatus.JSON


-- TODO: refactor queries to include returning types (using runQuery)

storepathQuery :: Query StorepathReadColumns
storepathQuery = queryTable storepathTable

existsStorePath :: BS.ByteString -> Query (Column PGText)
existsStorePath path = proc () -> do
    Storepath{..} <- storepathQuery -< ()
    restrict -< storepathPath .== pgString (BS.unpack path)
    returnA -< storepathPath

insertStorepath :: PG.Connection -> BS.ByteString -> IO (Maybe Int32)
insertStorepath conn path =
  fmap listToMaybe $ runInsertManyReturning conn storepathTable [sp] storepathId
    where
      sp = Storepath
        { storepathId = Nothing
        , storepathPath = pgString $ BS.unpack path
        }

insertStorepathcontents :: PG.Connection -> Directory -> Int32 -> IO Int64
insertStorepathcontents conn dir sp =
  runInsertMany conn storepathcontentTable $ dirToPG dir [] sp

searchFileQuery :: PG.Connection -> Maybe String -> [File]
searchFileQuery conn s = undefined

-- Helpers

dirToPG :: Directory -> [Text] -> Int32 -> [StorepathcontentWriteColumns]
dirToPG (Directory lst) fp sp = concatMap f lst
  where
    f (DirectoryEntry path dir) = dirToPG dir (fp ++ [path]) sp
dirToPG (FilePath file (File size isexecutable)) fp sp =
  [Storepathcontent
    { storepathcontentStorepath = pgInt4 $ fromIntegral sp
    , storepathcontentSubpath = pgStrictText $ intercalate "/" fp
    , storepathcontentFile = pgStrictText file
    , storepathcontentType = pgInt4 $ typeToInt "regular"
    , storepathcontentIsexecutable = pgBool $ toBool isexecutable
    , storepathcontentFilesize = Just $ toNullable $ pgInt4 $ fromIntegral size
    , storepathcontentTarget = Nothing
    }]
dirToPG (Symlink file target) fp sp =
  [Storepathcontent
    { storepathcontentStorepath = pgInt4 $ fromIntegral sp
    , storepathcontentSubpath = pgStrictText $ intercalate "/" fp
    , storepathcontentFile = pgStrictText $ file
    , storepathcontentType = pgInt4 $ typeToInt "symlink"
    , storepathcontentIsexecutable = pgBool False
    , storepathcontentFilesize = Nothing
    , storepathcontentTarget = Just $ toNullable $ pgStrictText target
    }]


typeToInt :: String -> Int
typeToInt "regular" = 0
typeToInt "symlink" = 1

toBool :: IsExecutable -> Bool
toBool Executable = True
toBool NotExecutable = False






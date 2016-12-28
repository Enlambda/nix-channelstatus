{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ChannelStatus.Query where

import           Control.Arrow (returnA)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text, intercalate)
import           Data.Maybe (listToMaybe, fromMaybe)
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Int
import           Opaleye

import           ChannelStatus.Database
import           ChannelStatus.JSON


-- Storepath

storepathQuery :: Query StorepathReadColumns
storepathQuery = queryTable storepathTable

existsStorePathQuery :: BS.ByteString -> Query (Column PGText)
existsStorePathQuery path = proc () -> do
    Storepath{..} <- storepathQuery -< ()
    restrict -< storepathPath .== pgString (BS.unpack path)
    returnA -< storepathPath

existsStorePath :: PG.Connection -> BS.ByteString -> IO [Text]
existsStorePath conn path = runQuery conn $ existsStorePathQuery path

insertStorepath :: PG.Connection -> BS.ByteString -> IO (Maybe Int32)
insertStorepath conn path =
  fmap listToMaybe $ runInsertManyReturning conn storepathTable [sp] storepathId
    where
      sp = Storepath
        { storepathId = Nothing
        , storepathPath = pgString $ BS.unpack path
        }

-- Storepathcontents

storepathcontentsQuery :: Query StorepathcontentReadColumns
storepathcontentsQuery = queryTable storepathcontentTable

insertStorepathcontents :: PG.Connection -> Directory -> Int32 -> IO Int64
insertStorepathcontents conn dir sp =
  runInsertMany conn storepathcontentTable $ dirToPG dir [] sp

spcQuery :: Maybe Text -> Query StorepathcontentReadColumns
spcQuery q = proc () -> do
  row@Storepathcontent{..} <- storepathcontentsQuery -< ()
  restrict -< like storepathcontentFile $ pgStrictText $ fromMaybe "" q
  restrict -< in_ ["bin", "sbin"] storepathcontentSubpath
  returnA -< row

searchBinariesQuery :: Maybe Text -> Query (StorepathcontentReadColumns, StorepathReadColumns)
searchBinariesQuery q = proc () -> do
  sp@Storepath{..} <- storepathQuery -< ()
  spc@Storepathcontent{..} <- (spcQuery q) -< ()
  -- TODO: limit
  restrict -< storepathcontentStorepath .== storepathId
  returnA -< (spc, sp)


searchBinaries :: PG.Connection -> Maybe Text -> IO [(Storepathcontent, Storepath)]
searchBinaries conn s =
  runQuery conn $ searchBinariesQuery s


-- Helpers

dirToPG :: Directory -> [Text] -> Int32 -> [StorepathcontentWriteColumns]
dirToPG (Directory lst) fp sp = concatMap f lst
  where
    f (DirectoryEntry path dir) = dirToPG dir (fp ++ [path]) sp
dirToPG (FilePath file (File size isexecutable)) fp sp =
  [Storepathcontent
    { storepathcontentStorepath = pgInt4 $ fromIntegral sp
    , storepathcontentSubpath = pgStrictText $ intercalate "/" (init fp)
    , storepathcontentFile = pgStrictText file
    , storepathcontentType = pgInt4 $ typeToInt "regular"
    , storepathcontentIsexecutable = pgBool $ toBool isexecutable
    , storepathcontentFilesize = Just $ toNullable $ pgInt4 $ fromIntegral size
    , storepathcontentTarget = Nothing
    }]
dirToPG (Symlink file target) fp sp =
  [Storepathcontent
    { storepathcontentStorepath = pgInt4 $ fromIntegral sp
    , storepathcontentSubpath = pgStrictText $ intercalate "/" (init fp)
    , storepathcontentFile = pgStrictText $ file
    , storepathcontentType = pgInt4 $ typeToInt "symlink"
    , storepathcontentIsexecutable = pgBool False
    , storepathcontentFilesize = Nothing
    , storepathcontentTarget = Just $ toNullable $ pgStrictText target
    }]

-- TODO: refactor these

typeToInt :: String -> Int
typeToInt "regular" = 0
typeToInt "symlink" = 1
typeToInt _ = 2

toBool :: IsExecutable -> Bool
toBool Executable = True
toBool NotExecutable = False






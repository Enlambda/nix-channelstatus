{-# LANGUAGE OverloadedStrings #-}

{- Implements https://github.com/NixOS/nixos-channel-scripts/blob/master/generate-programs-index.cc
   in Haskell using parallelization.
-}

module ChannelStatus.IndexFiles
    ( indexChannelFiles
    ) where

import Data.Aeson (eitherDecode)
import Control.Concurrent.ParallelIO.Local
import Control.Monad (when, void)
import Data.Conduit.Lzma as L
import Data.Conduit.List (consume)
import Data.Maybe (catMaybes)
import Data.Either ()
import Data.Monoid ((<>))
import Data.Text (Text)
import           GHC.Int
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Conduit
import Opaleye
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)

import ChannelStatus.JSON (Directory)
import ChannelStatus.Query

-- Example: https://cache.nixos.org/626rn6vqqv1pbrlidwyb2ddppmb9hm38.ls.xz


pathToURL :: BS.ByteString -> BS.ByteString
pathToURL path =
  "https://cache.nixos.org/" <> hash <> ".ls.xz"
    where
      hash = BS.take 32 $ BS.drop 11 path

retrieveXZ :: Manager -> String -> IO [BS.ByteString]
retrieveXZ manager url =
  runResourceT $ do
    request <- parseRequest url
    response <- http request manager
    responseBody response C.$=+ L.decompress Nothing C.$$+- consume

transform :: Manager -> BS.ByteString -> IO ()
transform manager path = do
  conn <- PG.connectPostgreSQL "postgresql:///channelstatus"
  paths <- existsStorePath conn path
  when (paths == []) $ PG.withTransaction conn $ do
    -- Index since path is not known yet
    storepathId <- insertStorepath conn path
    xz <- retrieveXZ manager $ BS.unpack $ pathToURL path
    indexStorePath conn path storepathId $ eitherDecode . BSL.fromStrict $ BS.concat xz
  PG.close conn

indexStorePath :: PG.Connection -> BS.ByteString -> Maybe Int32 -> Either String Directory -> IO ()
indexStorePath _ path _ (Left msg) =
  print $ "Error during parsing of " <> BS.unpack path <> " : " <> msg
indexStorePath _ path Nothing (Right dir) =
  print $ "No storepathID for " <> BS.unpack path
indexStorePath conn path (Just storepathId) (Right dir) =
  void $ insertStorepathcontents conn dir storepathId


indexChannelFiles :: String -> IO ()
indexChannelFiles url = do
  -- TODO: HTTP2 would speed this up
  manager <- newManager tlsManagerSettings

  -- get store-paths.xz
  tempStorePaths <- retrieveXZ manager url

  -- get list of store paths
  let storePaths = BS.lines $ BS.concat $ tempStorePaths


  -- http://hackage.haskell.org/package/stm-conduit-0.2.4.1/docs/Data-Conduit-TMChan.html#v:sinkTBMChan
  -- for each hash, request the *.ls.xz and parse it
  exceptions <- withPool 40 $ \pool ->
    parallelE_ pool $ fmap (transform manager) storePaths
    -- TODO: figure out system and attribute (evaluate nixpkgs)
    -- TODO: retry the failed
-- "/nix/store/d4whi0j8sx07j8fyjb08d2xx4cf4h3p5-aspell-dict-nb-0.50.1-0"
--Left "Error in $: Failed reading: Cannot decode byte '\\x6c': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
  print $ catMaybes exceptions

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONSpec (main, spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import Text.RawString.QQ
import Test.Hspec

import ChannelStatus.JSON


example1 = [r|
{
    "version": 1,
    "root": {
      "type": "directory",
      "entries": {
        "lib": {
          "type": "directory",
          "entries": {
            "Mcrt1.o": {
              "type": "regular",
              "size": 1288
            },
            "Scrt1.o": {
              "type": "regular",
              "size": 3920
            }
          }
        }
      }
    }
  }
|]

example2 = [r|
{"version":1,"root":{"type":"directory","entries":{"bin":{"type":"directory","entries":{"jabref":{"type":"regular","size":266,"executable":true}}},"share":{"type":"directory","entries":{"applications":{"type":"directory","entries":{"jabref.desktop":{"type":"regular","size":210,"executable":true}}},"icons":{"type":"directory","entries":{"jabref.svg":{"type":"regular","size":9227,"executable":true}}},"java":{"type":"directory","entries":{"jabref-3.6.jar":{"type":"symlink","target":"/nix/store/wcdgkb878cjkw6x7nd9gfa6q5jpv219m-JabRef-3.6.jar"}}}}}}}}
|]

example1Directory =
  Directory [DirectoryEntry "lib" (Directory [
    DirectoryEntry "Mcrt1.o" (FilePath "Mcrt1.o" (File 1288 NotExecutable)),
    DirectoryEntry "Scrt1.o" (FilePath "Scrt1.o" (File 3920 NotExecutable))])]

example2Directory =
  Directory [DirectoryEntry "bin" (Directory [DirectoryEntry "jabref" (FilePath "jabref" (File 266 Executable))]),DirectoryEntry "share" (Directory [DirectoryEntry "java" (Directory [DirectoryEntry "jabref-3.6.jar" (Symlink "jabref-3.6.jar" "/nix/store/wcdgkb878cjkw6x7nd9gfa6q5jpv219m-JabRef-3.6.jar")]),DirectoryEntry "icons" (Directory [DirectoryEntry "jabref.svg" (FilePath "jabref.svg" (File 9227 Executable))]),DirectoryEntry "applications" (Directory [DirectoryEntry "jabref.desktop" (FilePath "jabref.desktop" (File 210 Executable))])])]

spec :: Spec
spec = do
  describe "parsing" $ do
    it "exampleJSON" $ do
      (eitherDecode example1 :: Either String Directory) `shouldBe` Right example1Directory
    it "example2" $ do
      (eitherDecode example2 :: Either String Directory) `shouldBe` Right example2Directory


main :: IO ()
main = hspec spec

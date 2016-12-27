module Main where

import ChannelStatus.IndexFiles

main :: IO ()
main = indexChannelFiles "https://d3g5gsiof5omrk.cloudfront.net/nixos/16.09/nixos-16.09.975.1e1112e/store-paths.xz"
--someFunc = indexStorePaths "https://d3g5gsiof5omrk.cloudfront.net/nixos/16.09-small/nixos-16.09.1017.030ffa9/store-paths.xz"
-- TODO: remember if particular channel was already indexed

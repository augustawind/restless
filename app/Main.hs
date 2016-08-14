module Main where

import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as S8

import Restless

main :: IO ()
main = S8.putStrLn (Yaml.encode defaultJSONRequest)

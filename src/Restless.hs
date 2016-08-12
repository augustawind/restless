{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Restless
    ( JSONRequest
    , defaultJSONRequest
    , someFunc
    ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Client as Http

-----------------------------------------------------------------------
--  JSONRequest

data JSONRequest = JSONRequest
    { host :: T.Text
    , method :: T.Text
    , secure :: Bool
    , port :: Int
    , path :: T.Text
    , queryString :: T.Text
    , requestHeaders :: Object
    , requestBody :: Object
    } deriving (Show, Read, Eq)

instance FromJSON JSONRequest where
    parseJSON = withObject "JSONRequest" $ \o -> do
        host            <- o .:  "host"
        method          <- o .:? "method"           .!= "GET"
        secure          <- o .:? "secure"           .!= False
        port            <- o .:? "port"             .!= 80
        path            <- o .:? "path"             .!= "/"
        queryString     <- o .:? "queryString"      .!= ""
        requestHeaders  <- o .:? "requestHeaders"   .!= HM.empty
        requestBody     <- o .:? "requestBody"      .!= HM.empty
        return JSONRequest {..}

instance ToJSON JSONRequest where
    toJSON r = object
        [ "host"            .= host r
        , "method"          .= method r
        , "secure"          .= secure r
        , "port"            .= port r
        , "path"            .= path r
        , "queryString"     .= queryString r
        , "requestHeaders"  .= requestHeaders r
        , "requestBody"     .= requestBody r
        ]

defaultJSONRequest = JSONRequest
    { host              = "localhost"
    , method            = "GET"
    , secure            = False
    , port              = 80
    , path              = "/"
    , queryString       = ""
    , requestHeaders    = HM.empty
    , requestBody       = HM.empty
    }

someFunc :: IO ()
someFunc = print "notimplemented"

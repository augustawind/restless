{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Restless.Request
    ( JSONRequest(..)
    , defaultJSONRequest
    ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-----------------------------------------------------------------------
-- Data declarations

data JSONRequest = JSONRequest
    { host :: S8.ByteString
    , method :: S8.ByteString
    , secure :: Bool
    , port :: Int
    , path :: S8.ByteString
    , queryString :: [(S8.ByteString, Maybe S8.ByteString)]
    , requestHeaders :: Object
    , requestBody :: Object
    } deriving (Show, Read, Eq)

defaultJSONRequest = JSONRequest
    { host              = "localhost"
    , method            = "GET"
    , secure            = False
    , port              = 80
    , path              = "/"
    , queryString       = []
    , requestHeaders    = HM.empty
    , requestBody       = HM.empty
    }

------------------------------------------------------------------------
--  JSON instances

instance FromJSON JSONRequest where
    parseJSON = withObject "JSONRequest" $ \o -> do
        host            <- o .:  "host"
        method          <- o .:? "method"           .!= "GET"
        secure          <- o .:? "secure"           .!= False
        port            <- o .:? "port"             .!= 80
        path            <- o .:? "path"             .!= "/"
        queryString     <- o .:? "queryString"      .!= []
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

instance FromJSON S8.ByteString where
    parseJSON = withText "Strict Char8 ByteString" (return . encodeUtf8)

instance ToJSON S8.ByteString where
    toJSON = String . decodeUtf8

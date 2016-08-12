{-# LANGUAGE OverloadedStrings #-}
-- Needed for the ToJSON and FromJSON instances of CI S8.ByteString
{-# LANGUAGE FlexibleInstances #-}
module Restless
    ( someFunc
    ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Data.CaseInsensitive (CI, mk, original)
import qualified Data.Yaml as Yaml
import Network.HTTP.Client

-- Setup instances for serializing/deserializing Requests to/from JSON.

instance FromJSON L8.ByteString where
    parseJSON = withText "ByteString.Lazy" (return . L8.pack . T.unpack)

instance ToJSON L8.ByteString where
    toJSON = String . T.pack . L8.unpack

instance FromJSON S8.ByteString where
    parseJSON = withText "ByteString" (return . S8.pack . T.unpack)

instance ToJSON S8.ByteString where
    toJSON = String . T.pack . S8.unpack

instance FromJSON (CI S8.ByteString) where
    parseJSON =  withText "CI ByteString.Lazy" (return . mk . S8.pack . T.unpack)

instance ToJSON (CI S8.ByteString) where
    toJSON = String . T.pack . S8.unpack . original

instance FromJSON Proxy where
    parseJSON = withObject "Proxy" $ \o ->
        Proxy <$> o .: "proxyHost" <*> o .: "proxyPort"

instance ToJSON Proxy where
    toJSON p = object
        [ "proxyHost" .= toJSON (proxyHost p)
        , "proxyPort" .= toJSON (proxyPort p)
        ]

instance FromJSON RequestBody where
    parseJSON = withText "RequestBody" $
        return . RequestBodyLBS . L8.pack . T.unpack

instance ToJSON RequestBody where
    toJSON r = case r of
                 RequestBodyLBS l8 -> toJSON l8
                 RequestBodyBS s8  -> toJSON s8
                 _ -> error "ToJSON RequestBody only supports\
                            \RequestBodyLBS and RequestBodyBS"

instance FromJSON Request where
    parseJSON = withObject "Request" $ \o -> do
        method <- o .: "method"
        secure <- o .: "secure"
        host <- o .: "host"
        port <- o .: "port"
        path <- o .: "path"
        queryString <- o .: "queryString"
        requestHeaders <- o .: "requestHeaders"
        requestBody <- o .: "requestBody"
        return defaultRequest { method = method
                              , secure = secure
                              , host = host
                              , port = port
                              , path = path
                              , queryString = queryString
                              , requestHeaders = requestHeaders
                              , requestBody = requestBody
                              }

instance ToJSON Request where
    toJSON r = object [ "method" .= show (method r)
                      , "secure" .= secure r
                      , "host" .= host r
                      , "port" .= port r
                      , "path" .= path r
                      , "queryString" .= queryString r
                      , "requestHeaders" .= requestHeaders r
                      , "requestBody" .= requestBody r
                      , "proxy" .= proxy r
                      ]

someFunc :: IO ()
someFunc = do
    let r = Yaml.encode defaultRequest
    S8.putStrLn r
    let r' = Yaml.decodeEither r :: Either String Request
    print r'
    let r'' = Yaml.encode r'
    S8.putStrLn r''

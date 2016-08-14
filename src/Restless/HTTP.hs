module Restless.HTTP
    ( toHTTPRequest, toHTTPRequest'
    , doRequest, doRequestTLS
    ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import           Network.HTTP.Simple
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T

import Restless.Request

-- | Convert a JSONRequest to a Network.HTTP Request.
toHTTPRequest :: Manager -> JSONRequest -> Request
toHTTPRequest manager = setRequestManager manager . toHTTPRequest'

-- | Convert a JSONRequest to a Network.HTTP Request with the default Manager.
toHTTPRequest' :: JSONRequest -> Request
toHTTPRequest' r =
      setRequestHost (host r)
    . setRequestMethod (method r)
    . setRequestSecure (secure r)
    . setRequestPort (port r)
    . setRequestPath (path r)
    . setRequestQueryString (queryString r)
    . setRequestHeaders (requestHeaders r)
    . setRequestBodyJSON (requestBody r)
    $ defaultRequest

-- | Given a JSONRequest, perform an HTTP request, returning the response.
doRequest :: Manager -> JSONRequest -> IO (Response Value)
doRequest manager = httpJSON . toHTTPRequest manager

-- | Convenience function that performs `doRequest` with a new TLS manager.
doRequestTLS :: JSONRequest -> IO (Response Value)
doRequestTLS r = do
    manager <- newManager tlsManagerSettings
    doRequest manager r 

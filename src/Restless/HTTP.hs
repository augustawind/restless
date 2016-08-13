module Restless.HTTP
    ( toRequest
    , Http.httpJSON
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.Conduit (($$))
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Map as M
import           Network.HTTP.Simple
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import Restless.Request

-- | Convert a JSONRequest to a Network.HTTP Request.
toRequest :: JSONRequest -> Request
toRequest r =
      setRequestHost (host r)
    . setRequestMethod (method r)
    . setRequestSecure (secure r)
    . setRequestPort (port r)
    . setRequestPath (path r)
    . setRequestQueryString (queryString r)
    . setRequestHeaders $ M.toList
                          . M.mapKeys (CI.mk . encodeUtf8)
                          . M.map encodeUtf8 
                          $ requestHeaders r
    . setRequestBodyJSON (requestBody r)
    $ defaultRequest

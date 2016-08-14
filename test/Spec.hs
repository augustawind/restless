{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (void)
import Data.Aeson (encode, decode, object, (.=))
import Network.HTTP.Simple
import qualified Network.HTTP.Client as H
import Test.HUnit hiding (path)

import Restless

test_jsonRequestToAndFromJSONInstances = TestCase $ do
    let req = defaultJSONRequest
        x = encode req
        y = decode x :: Maybe JSONRequest
        z = encode y
    assertEqual "Response should be serializable to and from JSON" x z

test_toHTTPRequest = TestList
    [ "localhost" ~=? H.host r
    , "DELETE" ~=? H.method r
    , True ~=? H.secure r
    , 8080 ~=? H.port r
    , "/" ~=? H.path r
    , [("foo", Just "bar")] ~=? getRequestQueryString r
    , ["application/json"] ~=? getRequestHeader "content-type" r
    , encode body ~=? let (H.RequestBodyLBS b) = H.requestBody r
                       in b
    ]
        where r = toHTTPRequest' jsonRequest
              jsonRequest = JSONRequest
                  { host = "localhost"
                  , method = "DELETE"
                  , secure = True
                  , port = 8080
                  , path = "/"
                  , queryString = [("foo", Just "bar")] 
                  , requestHeaders = [("content-type", "application/json")]
                  , requestBody = body
                  }
              body = object [ "name" .= ("dustin" :: String)
                            , "age"  .= (24 :: Int)
                            ]

tests = TestList
    [ test_jsonRequestToAndFromJSONInstances
    , test_toHTTPRequest
    ]

main :: IO ()
main = void $ runTestTT tests

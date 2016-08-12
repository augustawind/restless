import Control.Monad (void)
import Data.Aeson (encode, decode)
import Test.HUnit

import Restless

testRequestEncodeDecode = TestCase $ do
    let req = defaultJSONRequest
        x = encode req
        y = decode x :: Maybe JSONRequest
        z = encode y
    assertEqual "Response should be serializable to and from JSON" x z

tests = TestList [testRequestEncodeDecode]

main :: IO ()
main = void $ runTestTT tests

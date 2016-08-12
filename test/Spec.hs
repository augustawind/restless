import Control.Monad (void)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Char8 as S8
import Test.HUnit
import Network.HTTP.Client (Request, defaultRequest)

import Restless

testRequestEncodeDecode = TestCase $ do
    let x = encode defaultRequest
        y = decode x :: Maybe Request
        z = encode y
    assertEqual "Response should be serializable to and from JSON" x z

tests = TestList [testRequestEncodeDecode]

main :: IO ()
main = void $ runTestTT tests

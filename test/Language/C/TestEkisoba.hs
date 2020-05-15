{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Language.C.Ekisoba as Eki
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList [testSample]
    return ()

testSample :: Test
testSample = TestList
    [ "testSample test 1" ~: id "hello" ~?= "hello"
    ]

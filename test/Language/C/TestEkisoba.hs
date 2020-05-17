{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Aeson                  as Aes
import qualified Data.Aeson.Encode.Pretty    as AesP
import qualified Data.Aeson.TH               as TH
import qualified Data.Text                   as T
import qualified Language.C.Data.InputStream as IS
import qualified Language.C.Data.Position    as Pos
import qualified Language.C.Ekisoba          as Eki
import           Language.C.Ekisoba.AST
import qualified Language.C.Parser           as Pars
import qualified Language.C.Syntax.AST       as AST
import qualified System.IO                   as SIO
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList [testSample, testTranslate]
    return ()

testSample :: Test
testSample = TestList
    [ "testSample test 1" ~: "hello" ~?= "hello"
    ]

testTranslate :: Test
testTranslate = TestList
    [ "testTranslate test 1" ~: helper "./hoge.c" input 0 ~?= (expected !! 0)
    , "testTranslate test 2" ~: helper "./hoge.c" input 1 ~?= (expected !! 1)
    , "testTranslate test 3" ~: helper "./hoge.c" input 2 ~?= (expected !! 2)
    ]
  where
    helper path inp n = (string . Eki.translate . cToOriginAst path) (inp !! n)
    input = [
          [r|int hoge;|]
        , [r|int hoge = 123;|]
        , [r|int hoge = 123;char fuga = 'k';|]
        ]
    expected = [
          [r|int hoge;|]
        , [r|int hoge = 123;|]
        , [r|int hoge = 123;
char fuga = 'k';|]
        ]


cToOriginAst :: FilePath -> IS.InputStream -> AST.CTranslUnit
cToOriginAst file text = case Pars.parseC text (Pos.initPos file) of
        Left _  -> error "parse error"
        Right r -> r

--writeTempFile :: T.Text -> IO AST.CTranslUnit
--writeTempFile _ = do
--    (path, handle) <- SIO.openTempFile "." "ekisoba-test"


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Text                   as T
import qualified Language.C.Data.InputStream as IS
import qualified Language.C.Data.Position    as Pos
import qualified Language.C.Ekisoba          as Eki
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
    [ "testSample test 1" ~: id "hello" ~?= "hello"
    ]

testTranslate :: Test
testTranslate = TestList
    [ "testTranslate test 1" ~: (show . cToOriginAst ".") input1 ~?= "hello"
    ]
  where
    input1 = [r|int hoge;|]

cToOriginAst :: FilePath -> IS.InputStream -> AST.CTranslUnit
cToOriginAst file text = case Pars.parseC text (Pos.initPos file) of
        Left _  -> error "parse error"
        Right r -> r

--writeTempFile :: T.Text -> IO AST.CTranslUnit
--writeTempFile _ = do
--    (path, handle) <- SIO.openTempFile "." "ekisoba-test"


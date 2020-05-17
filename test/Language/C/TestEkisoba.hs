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
testTranslate = TestList $ map helper testTable
  where
    helper (comment, path, input, expected) =
        comment ~: (string . Eki.translate . cToOriginAst path) input ~?= expected
    testTable = [ ("testTranslate test 1", "./hoge.c"
                  , [r|int hoge;|]
                  , [r|int hoge;|])
                , ("testTranslate test 2", "./hoge.c"
                  , [r|int hoge = 123; char fuga = 'k';|]
                  , [r|int hoge = 123;
char fuga = 'k';|])
                , ("testTranslate test 3", "./hoge.c"
                  , [r|char var_a, var_b = 154;|]
                  , [r|char var_a;
char var_b = 154;|])
                ]

cToOriginAst :: FilePath -> IS.InputStream -> AST.CTranslUnit
cToOriginAst file text = case Pars.parseC text (Pos.initPos file) of
        Left _  -> error "parse error"
        Right r -> r


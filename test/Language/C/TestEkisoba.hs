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
    runTestTT $ TestList [ testSample
                         , testVariableDefinition
                         , testFunctionDefinition
                         , testExpression
                         ]
    return ()

helper (comment, path, input, expected) =
    comment ~: ((\r -> case r of
        Right r' -> string r'
        Left l   -> error $ Eki.message l) . Eki.translate . cToOriginAst path) input ~?= expected

testSample :: Test
testSample = TestList
    [ "testSample test 1" ~: "hello" ~?= "hello"
    ]

testVariableDefinition :: Test
testVariableDefinition = TestList $ map helper testTable
  where
    testTable = [ ("test variable definition 1", "./hoge.c"
                  , [r|int hoge;|]
                  , [r|int hoge;|])
                , ("test variable definition 2", "./hoge.c"
                  , [r|int hoge = 123; char fuga = 'k';|]
                  , [r|int hoge = 123;
char fuga = 'k';|])
                , ("test variable definition 3", "./hoge.c"
                  , [r|char var_a, var_b = 154;|]
                  , [r|char var_a;
char var_b = 154;|])
                , ("test variable definition 4", "./hoge.c"
                  , [r|static unsigned int var_s_uint = 555;|]
                  , [r|static unsigned int var_s_uint = 555;|])
                , ("test variable definition 5", "./hoge.c"
                  , [r|char var_c;
unsigned char var_uc;
signed char var_sc;
short var_s;
unsigned short var_us;
signed short var_ss;
int var_i;
unsigned int var_ui;
signed int var_si;
long var_l;
unsigned long var_ul;
signed long var_sl;
long long var_ll;
unsigned long long var_ull;
signed long long var_sll;
float var_f;
double var_d;
signed long long var_sll;|]
                  , [r|char var_c;
unsigned char var_uc;
signed char var_sc;
short var_s;
unsigned short var_us;
signed short var_ss;
int var_i;
unsigned int var_ui;
signed int var_si;
long var_l;
unsigned long var_ul;
signed long var_sl;
long long var_ll;
unsigned long long var_ull;
signed long long var_sll;
float var_f;
double var_d;
signed long long var_sll;|]
                  )
                , ("test variable definition 4", "./hoge.c"
                  , [r|char *p_var;
char **pp_var;
char ***ppp_var;|]
                  , [r|char * p_var;
char * * pp_var;
char * * * ppp_var;|])
                , ("test variable definition 4", "./hoge.c"
                  , [r|static int hoge; const int fuga; volatile int piyo;|]
                  , [r|static int hoge;
const int fuga;
volatile int piyo;|])
                , ("test variable definition struct", "./hoge.c"
                  , [r|struct St_tag {
  int menber_i;
  char menber_c;
};|]
                  , [r|struct St_tag {
    int menber_i;
    char menber_c;
};|])
                ]

testFunctionDefinition :: Test
testFunctionDefinition = TestList $ map helper testTable
  where
    testTable = [ ("test function definition 1", "./hoge.c"
                  , [r|void func(void){}
void func2(int a, char b){}|]
                  , [r|void func(void)
{

}
void func2(int a, char b)
{

}|])
                ]

cToOriginAst :: FilePath -> IS.InputStream -> AST.CTranslUnit
cToOriginAst file text = case Pars.parseC text (Pos.initPos file) of
        Left _  -> error "parse error"
        Right r -> r

testExpression :: Test
testExpression = TestList $ map helper testTable
  where
    testTable = [ ("test expression add", "./hoge.c"
                  , [r|int hoge = 1 + 2;|]
                  , [r|int hoge = 1 + 2;|])
                , ("test expression sub", "./hoge.c"
                  , [r|int hoge = 1 - 2;|]
                  , [r|int hoge = 1 - 2;|])
                , ("test expression mul", "./hoge.c"
                  , [r|int hoge = 1 * 2;|]
                  , [r|int hoge = 1 * 2;|])
                , ("test expression div", "./hoge.c"
                  , [r|int hoge = 2 / 1;|]
                  , [r|int hoge = 2 / 1;|])
                , ("test expression nest 1", "./hoge.c"
                  , [r|int hoge = (1 + 2) * (3 - 4);|]
                  , [r|int hoge = (1 + 2) * (3 - 4);|])
                , ("test expression nest 2", "./hoge.c"
                  , [r|int hoge = ((1 + 2) + (3 - 4)) * ((5 + 6) - (7 - 8));|]
                  , [r|int hoge = ((1 + 2) + (3 - 4)) * ((5 + 6) - (7 - 8));|])
                ]

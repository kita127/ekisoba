import qualified Data.Aeson                    as Aes
import qualified Data.Aeson.Encode.Pretty      as AesP
import qualified Data.Aeson.TH                 as TH
import qualified Data.Text                     as T
import qualified Language.C.Data.InputStream   as IS
import qualified Language.C.Data.Position      as Pos
import qualified Language.C.Ekisoba            as Eki
import           Language.C.Ekisoba.AST
import qualified Language.C.Parser             as Pars
import qualified Language.C.Syntax.AST         as AST
import qualified System.IO                     as SIO
import           Test.HUnit
import           Text.RawString.QQ


main :: IO ()
main = do
    runTestTT $ TestList
        [ testSample
        , testVariableDefinition
        , testFunctionDefinition
        , testStatement
        , testExpression
        , testTypedef
        ]
    return ()

helper (comment, path, input, expected) =
    comment
        ~:  ( (\r -> case r of
                  Right r' -> string 0 None r'
                  Left  l  -> error $ Eki.message l
              )
            . Eki.translate
            . cToOriginAst path
            )
                input
        ~?= expected

cToOriginAst :: FilePath -> IS.InputStream -> AST.CTranslUnit
cToOriginAst file text = case Pars.parseC text (Pos.initPos file) of
    Left  _ -> error "parse error"
    Right r -> r

-- | testSample
--
testSample :: Test
testSample = TestList ["testSample test 1" ~: "hello" ~?= "hello"]

-- | testVariableDefinition
--
testVariableDefinition :: Test
testVariableDefinition = TestList $ map helper testTable
  where
    testTable =
        [ ( "test variable definition 1"
          , "./hoge.c"
          , [r|int hoge;|]
          , [r|int hoge;|]
          )
        , ( "test variable definition 2"
          , "./hoge.c"
          , [r|int hoge = 123; char fuga = 'k';|]
          , [r|int hoge = 123;
char fuga = 'k';|]
          )
        , ( "test variable definition 3"
          , "./hoge.c"
          , [r|char var_a, var_b = 154;|]
          , [r|char var_a;
char var_b = 154;|]
          )
        , ( "test variable definition 4"
          , "./hoge.c"
          , [r|static unsigned int var_s_uint = 555;|]
          , [r|static unsigned int var_s_uint = 555;|]
          )
        , ( "test variable definition 5"
          , "./hoge.c"
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
        , ( "test variable definition 4"
          , "./hoge.c"
          , [r|char *p_var;
char **pp_var;
char ***ppp_var;|]
          , [r|char * p_var;
char * * pp_var;
char * * * ppp_var;|]
          )
        , ( "test variable definition 4"
          , "./hoge.c"
          , [r|static int hoge; const int fuga; volatile int piyo;|]
          , [r|static int hoge;
const int fuga;
volatile int piyo;|]
          )
        , ( "test variable definition struct"
          , "./hoge.c"
          , [r|struct St_tag {
  int menber_i;
  char menber_c;
};
struct St_tag st_var;|]
          , [r|struct St_tag {
    int menber_i;
    char menber_c;
};
struct St_tag st_var;|]
          )
        ]

-- | testFunctionDefinition
--
testFunctionDefinition :: Test
testFunctionDefinition = TestList $ map helper testTable
  where
    testTable =
        [ ( "test function definition 1"
          , "./hoge.c"
          , [r|void func(void){}
void func2(int a, char b){}|]
          , [r|void func(void)
{

}
void func2(int a, char b)
{

}|]
          )
        , ( "test function definition 2"
          , "./hoge.c"
          , [r|int add(int a, int b){ int res; res = a + b; return res; }|]
          , [r|int add(int a, int b)
{
    int res;
    res = (a + b);
    return res;
}|]
          )
        , ( "test function definition 3"
          , "./hoge.c"
          , [r|int add(int a, int b){ int res; res = a + b; return res; }|]
          , [r|int add(int a, int b)
{
    int res;
    res = (a + b);
    return res;
}|]
          )
        ]

-- | testStatement
--
testStatement :: Test
testStatement = TestList $ map helper testTable
  where
    testTable =
        [ ( "test statemet if, else if, else"
          , "./hoge.c"
          , [r|
unsigned int if_gethan_0(int arg) {
    int res;

    if(arg == 0) {
        res = 0;
    }
    else if(arg > 0)
    {
        res = 1;
    }
    else
    {
        res = 2;
    }
    return res;
}
|]
          , [r|unsigned int if_gethan_0(int arg)
{
    int res;
    if(arg == 0)
    {
        res = 0;
    }
    else if(arg > 0)
    {
        res = 1;
    }
    else
    {
        res = 2;
    }
    return res;
}|]
          )
        , ( "test statement switch"
          , "./hoge.c"
          , [r|void func_switch(int arg)
{
    switch(arg)
    {
        case 0:
            111;
            break;
        case 1:
            222;
            break;
        default:
            999;
            break;
    }
}|]
          , [r|void func_switch(int arg)
{
    switch(arg)
    {
        case 0:
            111;
            break;
        case 1:
            222;
            break;
        default:
            999;
            break;
    }
}|]
          )
        , ( "test statement while"
          , "./hoge.c"
          , [r|void func_switch(int arg)
{

    while(1)
    {
        100;
        200;
        300;
    }
}|]
          , [r|void func_switch(int arg)
{
    while(1)
    {
        100;
        200;
        300;
    }
}|]
          )
        ]

testExpression :: Test
testExpression = TestList $ map helper testTable
  where
    testTable =
        [ ( "test expression add"
          , "./hoge.c"
          , [r|int hoge = 1 + 2;|]
          , [r|int hoge = 1 + 2;|]
          )
        , ( "test expression sub"
          , "./hoge.c"
          , [r|int hoge = 1 - 2;|]
          , [r|int hoge = 1 - 2;|]
          )
        , ( "test expression mul"
          , "./hoge.c"
          , [r|int hoge = 1 * 2;|]
          , [r|int hoge = 1 * 2;|]
          )
        , ( "test expression div"
          , "./hoge.c"
          , [r|int hoge = 2 / 1;|]
          , [r|int hoge = 2 / 1;|]
          )
        , ( "test expression nest 1"
          , "./hoge.c"
          , [r|int hoge = (1 + 2) * (3 - 4);|]
          , [r|int hoge = (1 + 2) * (3 - 4);|]
          )
        , ( "test expression nest 2"
          , "./hoge.c"
          , [r|int hoge = ((1 + 2) + (3 - 4)) * ((5 + 6) - (7 - 8));|]
          , [r|int hoge = ((1 + 2) + (3 - 4)) * ((5 + 6) - (7 - 8));|]
          )
        ]

testTypedef :: Test
testTypedef = TestList $ map helper testTable
  where
    testTable =
        [ ( "test typedef 1"
          , "./hoge.c"
          , [r|typedef int myint; myint hoge;|]
          , [r|typedef int myint;
myint hoge;|]
          )
        ]

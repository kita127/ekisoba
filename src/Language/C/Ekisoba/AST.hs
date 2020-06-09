module Language.C.Ekisoba.AST
    ( Object(..)
    , Program(..)
    , Statement(..)
    , Expression(..)
    , Instruction(..)
    , string
    )
where

import qualified Data.Aeson                    as Aes
import qualified Data.Aeson.TH                 as TH
import qualified Data.Text                     as T

class Stringble a where
 -- string :: depth -> instruction -> a -> T.Text
    string :: Int -> Instruction -> a -> T.Text

data Instruction = None | ElseIf
    deriving (Eq, Show)

data Object = Object {
                 name    :: T.Text
              ,  program :: Program
              } deriving (Eq, Show)

instance Stringble Object where
    string depth _ Object { name = n, program = p } = string depth None p


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)

instance Stringble Program where
    string depth _ (Program ss) = stringStatements depth ss


data Statement = VariableDefinition {
                   name  :: T.Text
                 , typ   :: [Statement] -- only Type
                 , value :: Maybe Expression
                 }
               | FunctionDefinition {
                   name    :: T.Text
                 , retType :: [Statement] -- only Type
                 , args    :: Statement    -- only Argument
                 , body    :: Statement    -- only BlockStatement
                 }
               | Argument {
                    vars :: [Statement]    -- only VariableDefinition
                 }
               | BlockStatement {
                   statements :: [Statement]
                 }
               | Type {
                   name :: T.Text
                 }
               | StructDeclaration {
                   name    :: T.Text
                 , menbers :: [Statement]  -- only VariableDefinition
                 }
               | TypdefDeclaration {
                   name    :: T.Text
                 , typ :: [Statement]      -- only Type
                 }
               | ReturnStatement {
                   value  :: Maybe Expression
                 }
               | ExpressionStatement {
                   exp    :: Expression
                 }
               | IfStatement {
                   condition   :: Expression
                 , consequence :: Statement          -- BlockStatement
                 , alternative :: Maybe Statement    -- BlockStatement or IfStatement
                 }
               | SwitchStatement {
                   condition   :: Expression
                 , block :: Statement                -- BlockStatement
                 }
               | DefaultStatement {
                   statement :: Statement
                 }
               | CaseStatement {
                   caseVal :: Expression
                 , statement :: Statement
                 }
               | WhileStatement {
                   condition :: Expression
                 , statement :: Statement
                 }
               | Break {
                 }
               | ASTStmtInfo {                 -- 内部制御用の特に意味のない文
                  info :: T.Text
                 }
               deriving (Eq, Show)

instance Stringble Statement where
    string depth _ var@VariableDefinition{} =
        nestText depth $ variableToStr depth var <> ";"
    string depth _ FunctionDefinition { name = n, retType = ts, args = ag, body = b }
        = T.intercalate " " (map (string depth None) ts)
            <> " "
            <> n
            <> "("
            <> string depth None ag
            <> ")\n"
            <> string depth None b
    string depth _ Argument { vars = vs } =
        if map (map (string depth None) . typ) vs == [["void"]]
            then "void"
            else T.intercalate ", " (map (variableToStr depth) vs)
    string depth _ Type { name = n } = n
    string depth _ StructDeclaration { name = n, menbers = ms } =
        nestText depth "struct "
            <> n
            <> " {\n"
            <> T.intercalate "\n" (map (string (depth + nestLevel) None) ms)
            <> nestText depth "\n};"
    string depth _ TypdefDeclaration { name = n, typ = ts } =
        "typedef "
            <> T.intercalate " " (map (string depth None) ts)
            <> " "
            <> n
            <> ";"
    string depth _ ReturnStatement { value = v } = case v of
        Just v' -> nestText depth $ "return " <> string depth None v' <> ";"
        Nothing -> nestText depth "return;"
    string depth _ ExpressionStatement { exp = e } =
        nestText depth $ string depth None e <> ";"
    string depth inst IfStatement { condition = cod, consequence = cons, alternative = alt }
        = nestText depth
            $  (if inst == ElseIf then "else if(" else "if(")
            <> string depth None cod
            <> ")\n"
            <> string depth None cons
            <> case alt of
                   Nothing                 -> ""
                   Just alt'@IfStatement{} -> "\n" <> string depth ElseIf alt'
                   Just alt'@BlockStatement{} ->
                       "\n"
                           <> nestText depth "else"
                           <> "\n"
                           <> string depth None alt'
    string depth _ b@BlockStatement { statements = ss } =
        nestText depth
            $  "{\n"
            <> T.intercalate "\n" (map (string (depth + nestLevel) None) ss)
            <> "\n"
            <> nestText depth "}"
    string depth _ SwitchStatement { condition = con, block = b } =
        nestText depth
            $  "switch("
            <> string depth None con
            <> ")"
            <> "\n"
            <> case b of
                   b@BlockStatement { statements = ss } ->
                       nestText depth "{\n"
                           -- 先頭要素は case 文となり一つの文を持っている
                           -- 他の分は何故か Block に紐づく
                           <> ( T.concat
                              . map
                                    (\s -> case s of
                                        d@DefaultStatement{} ->
                                            string depth None d
                                        c@CaseStatement{} ->
                                            string depth None c
                                        x ->
                                            string (depth + nestLevel * 2)
                                                   None
                                                   x
                                                <> "\n"
                                    )
                              $ ss
                              )
                           <> nestText depth "}"
                   _ -> "string Switch Statement ERROR"
    string depth _ DefaultStatement { statement = s } =
        nestText (depth + nestLevel) "default:"
            <> "\n"
            <> string (depth + nestLevel * 2) None s
            <> "\n"
    string depth _ CaseStatement { caseVal = cv, statement = s } =
        nestText (depth + nestLevel) "case "
            <> string depth None cv
            <> ":"
            <> "\n"
            <> string (depth + nestLevel * 2) None s
            <> "\n"
    string depth _ Break = nestText depth "break;"
    string depth _ WhileStatement { condition = cod, statement = stm } =
        nestText depth "while("
            <> string depth None cod
            <> ")"
            <> "\n"
            <> string depth None stm

data Expression = Identifire {
                    name :: T.Text
                  }
                | IntegerLiteral {
                    intVal :: Integer
                  }
                | CharLiteral {
                    charVal :: Char
                  }
                | InfixExpression {
                    left     :: Expression
                  , operator :: T.Text
                  , right    :: Expression
                  }
                deriving (Eq, Show)

instance Stringble Expression where
    string depth _ Identifire { name = n }       = n
    string depth _ IntegerLiteral { intVal = v } = T.pack . show $ v
    string depth _ CharLiteral { charVal = c }   = "'" <> T.singleton c <> "'"
    string depth _ InfixExpression { left = l, operator = op, right = r } =
        stringInParenthese depth l
            <> " "
            <> op
            <> " "
            <> stringInParenthese depth r

-- | stringStatements
--
stringStatements :: Int -> [Statement] -> T.Text
stringStatements depth ss = T.intercalate "\n" (map (string depth None) ss)

-- | stringInParenthese
--
stringInParenthese :: Int -> Expression -> T.Text
stringInParenthese depth InfixExpression { left = l, operator = op, right = r }
    = "("
        <> stringInParenthese depth l
        <> " "
        <> op
        <> " "
        <> stringInParenthese depth r
        <> ")"
stringInParenthese depth x = string depth None x

nestText :: Int -> T.Text -> T.Text
nestText depth s = T.pack (replicate depth ' ') <> s

valueToStr :: Int -> Maybe Expression -> T.Text
valueToStr depth (Just v) = " = " <> string depth None v
valueToStr depth Nothing  = ""

variableToStr :: Int -> Statement -> T.Text
variableToStr depth VariableDefinition { name = n, typ = ts, value = v } =
    T.intercalate " " (map (string depth None) ts)
        <> " "
        <> n
        <> valueToStr depth v

nestLevel :: Int
nestLevel = 4

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)
$(TH.deriveJSON TH.defaultOptions ''Expression)

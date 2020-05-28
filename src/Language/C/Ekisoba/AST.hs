module Language.C.Ekisoba.AST
    ( Object(..)
    , Program(..)
    , Statement(..)
    , Expression(..)
    , string
    )
where

import qualified Data.Aeson                    as Aes
import qualified Data.Aeson.TH                 as TH
import qualified Data.Text                     as T

class Stringble a where
    string :: Int -> a -> T.Text

data Object = Object {
                 name    :: T.Text
              ,  program :: Program
              } deriving (Eq, Show)

instance Stringble Object where
    string depth Object { name = n, program = p } = string depth p


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)

instance Stringble Program where
    string depth (Program ss) = T.intercalate "\n" $ map (string depth) ss


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
                  , alternative :: Maybe Statement    -- BlockStatement
                  }
               | ASTStmtInfo {                 -- 内部制御用の特に意味のない文
                  info :: T.Text
                 }
               deriving (Eq, Show)

instance Stringble Statement where
    string depth var@VariableDefinition{} =
        nestText depth $ variableToStr depth var <> ";"
    string depth FunctionDefinition { name = n, retType = ts, args = ag, body = b }
        = T.intercalate " " (map (string depth) ts)
            <> " "
            <> n
            <> "("
            <> string depth ag
            <> ")\n"
            <> string depth b
    string depth Argument { vars = vs } =
        if map (map (string depth) . typ) vs == [["void"]]
            then "void"
            else T.intercalate ", " (map (variableToStr depth) vs)
    string depth Type { name = n } = n
    string depth StructDeclaration { name = n, menbers = ms } =
        nestText depth "struct "
            <> n
            <> " {\n"
            <> T.intercalate "\n" (map (string (depth + nestLevel)) ms)
            <> nestText depth "\n};"
    string depth TypdefDeclaration { name = n, typ = ts } =
        "typedef "
            <> T.intercalate " " (map (string depth) ts)
            <> " "
            <> n
            <> ";"
    string depth ReturnStatement { value = v } = case v of
        Just v' -> nestText depth $ "return " <> string depth v' <> ";"
        Nothing -> nestText depth "return;"
    string depth ExpressionStatement { exp = e } =
        nestText depth $ string depth e <> ";"
    string depth IfStatement { condition = cod, consequence = cons, alternative = alt }
        = nestText depth
            $  "if("
            <> string depth cod
            <> ")\n"
            <> string depth cons
            <> case alt of
                   Nothing   -> ""
                   Just alt' -> string (depth + nestLevel) alt'
    string depth b@BlockStatement { statements = ss } =
        nestText depth
            $  "{\n"
            <> T.intercalate "\n" (map (string (depth + nestLevel)) ss)
            <> "\n"
            <> nestText depth "}"

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
    string depth Identifire { name = n }       = n
    string depth IntegerLiteral { intVal = v } = T.pack . show $ v
    string depth CharLiteral { charVal = c }   = "'" <> T.singleton c <> "'"
    string depth InfixExpression { left = l, operator = op, right = r } =
        stringInParenthese depth l
            <> " "
            <> op
            <> " "
            <> stringInParenthese depth r

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
stringInParenthese depth x = string depth x

nestText :: Int -> T.Text -> T.Text
nestText depth s = T.pack (replicate depth ' ') <> s

valueToStr :: Int -> Maybe Expression -> T.Text
valueToStr depth (Just v) = " = " <> string depth v
valueToStr depth Nothing  = ""

variableToStr :: Int -> Statement -> T.Text
variableToStr depth VariableDefinition { name = n, typ = ts, value = v } =
    T.intercalate " " (map (string depth) ts) <> " " <> n <> valueToStr depth v

nestLevel :: Int
nestLevel = 4

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)
$(TH.deriveJSON TH.defaultOptions ''Expression)

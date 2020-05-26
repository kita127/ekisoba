{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
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
    string :: a -> T.Text

data Object = Object {
                 name    :: T.Text
              ,  program :: Program
              } deriving (Eq, Show)

instance Stringble Object where
    string Object { name = n, program = p } = string p


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)

instance Stringble Program where
    string (Program ss) = T.intercalate "\n" $ map string ss


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
               | ASTStmtInfo {                 -- 内部制御用の特に意味のない文
                  info :: T.Text
                 }
               deriving (Eq, Show)

instance Stringble Statement where
    string var@VariableDefinition{} = variableToStr var <> ";"
    string FunctionDefinition { name = n, retType = ts, args = ag, body = b } =
        T.intercalate " " (map string ts)
            <> " "
            <> n
            <> "("
            <> string ag
            <> ")\n"
            <> string b
    string Argument { vars = vs } = if map (map string . typ) vs == [["void"]]
        then "void"
        else T.intercalate ", " (map variableToStr vs)
    string Type { name = n } = n
    string StructDeclaration { name = n, menbers = ms } =
        "struct "
            <> n
            <> " {\n"
            <> T.intercalate "\n" (map (nesting 4) ms)
            <> "\n};"
    string TypdefDeclaration { name = n, typ = ts } =
        "typedef " <> T.intercalate " " (map string ts) <> " " <> n <> ";"
    string ReturnStatement { value = v } = case v of
        Just v' -> "return " <> string v' <> ";"
        Nothing -> "return;"
    string ExpressionStatement { exp = e } = string e <> ";"
    string b@BlockStatement{}              = nestBlock 4 b
      where
        nestBlock :: Int -> Statement -> T.Text
        nestBlock nest BlockStatement { statements = ss } =
            "{\n" <> T.intercalate "\n" (map (nesting nest) ss) <> "\n}"

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
    string Identifire { name = n }       = n
    string IntegerLiteral { intVal = v } = T.pack . show $ v
    string CharLiteral { charVal = c }   = "'" <> T.singleton c <> "'"
    string InfixExpression { left = l, operator = op, right = r } =
        stringInParenthese l <> " " <> op <> " " <> stringInParenthese r

-- | stringInParenthese
--
stringInParenthese :: Expression -> T.Text
stringInParenthese InfixExpression { left = l, operator = op, right = r } =
    "("
        <> stringInParenthese l
        <> " "
        <> op
        <> " "
        <> stringInParenthese r
        <> ")"
stringInParenthese x = string x

nesting nest x = T.pack (replicate nest ' ') <> string x

valueToStr :: Maybe Expression -> T.Text
valueToStr (Just v) = " = " <> string v
valueToStr Nothing  = ""

variableToStr :: Statement -> T.Text
variableToStr VariableDefinition { name = n, typ = ts, value = v } =
    T.intercalate " " (map string ts) <> " " <> n <> valueToStr v

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)
$(TH.deriveJSON TH.defaultOptions ''Expression)

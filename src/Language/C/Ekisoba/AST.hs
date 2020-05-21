{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Ekisoba.AST
(
  Object(..)
, Program(..)
, Statement(..)
, string
) where

import qualified Data.Aeson    as Aes
import qualified Data.Aeson.TH as TH
import qualified Data.Text     as T

class Stringble a where
    string :: a -> T.Text

data Object = Object {
                 name    :: T.Text
              ,  program :: Program
              } deriving (Eq, Show)

instance Stringble Object where
    string Object{name = n, program = p} = string p


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)

instance Stringble Program where
    string (Program ss) = T.intercalate "\n" $ map string ss


data Statement = VariableDefinition {
                   name  :: T.Text
                 , typ   :: [T.Text]
                 , value :: Maybe T.Text
                 }
               | FunctionDefinition {
                   name    :: T.Text
                 , retType :: [T.Text]
                 , args    :: Statement  -- only Argument
                 , body    :: Statement    -- only BlockStatement
                 }
               | Argument {
                    vars :: [Statement]    -- only VariableDefinition
                 }
               | BlockStatement {
                   statements :: [Statement]
                 }
               deriving (Eq, Show)

instance Stringble Statement where
    string var@(VariableDefinition{}) = variableToStr var <> ";"
    string FunctionDefinition{name = n, retType = ts, args = ag, body = b} =
        T.intercalate " " ts <> " " <> n <> "(" <> string ag <> ")\n"
            <>  string b
    string Argument{vars = vs} =
        if map typ vs == [["void"]]
            then "void"
            else T.intercalate ", " (map variableToStr vs)
    string b@(BlockStatement{}) = nestBlock 4 b
      where
        nestBlock :: Int -> Statement -> T.Text
        nestBlock nest BlockStatement{statements = ss} =
                 "{\n" <> T.intercalate "\n" (map (nesting nest) ss) <> "\n}"

nesting nest x = T.pack (replicate nest ' ') <> string x

valueToStr :: Maybe T.Text -> T.Text
valueToStr (Just v) = " = " <> v
valueToStr Nothing  = ""

variableToStr :: Statement -> T.Text
variableToStr VariableDefinition{name = n, typ = ts, value = v} =
        T.intercalate " " ts <> " " <> n <> valueToStr v

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)

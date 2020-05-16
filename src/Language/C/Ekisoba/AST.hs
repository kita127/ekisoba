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
    string (Object{name = n, program = p}) = n <> "\n" <> string p


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)

instance Stringble Program where
    string (Program ss) = T.intercalate "\n" $ map string ss


data Statement = VariableDefinition {
                    name  :: T.Text
                 ,  typ   :: [T.Text]
                 ,  value :: Maybe T.Text
                 }
                 deriving (Eq, Show)

instance Stringble Statement where
    string (VariableDefinition{name = n, typ = ts, value = v}) =
        T.intercalate " " ts <> " " <> n <> toString v <> ";"
      where
        toString (Just v)  = " = " <> v
        toString (Nothing) = ""

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)

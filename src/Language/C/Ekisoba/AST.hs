{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.C.Ekisoba.AST
(
  Object(..)
, Program(..)
, Statement(..)
) where

import qualified Data.Aeson    as Aes
import qualified Data.Aeson.TH as TH
import qualified Data.Text     as T

data Object = Object {
                 objName :: T.Text
              ,  program :: Program
              } deriving (Eq, Show)


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)


data Statement = VariableDefinition {
                    varName  :: T.Text
                 ,  varType  :: [T.Text]
                 ,  varValue :: Maybe T.Text
                 }
                 deriving (Eq, Show)

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)

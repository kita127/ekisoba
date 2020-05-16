{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
                 name :: T.Text
              ,  program :: Program
              } deriving (Eq, Show)


newtype Program = Program {statements :: [Statement]} deriving (Eq, Show)


data Statement = VariableDefinition {
                    name  :: T.Text
                 ,  typ  :: [T.Text]
                 ,  value :: Maybe T.Text
                 }
                 deriving (Eq, Show)

$(TH.deriveJSON TH.defaultOptions ''Object)
$(TH.deriveJSON TH.defaultOptions ''Program)
$(TH.deriveJSON TH.defaultOptions ''Statement)

{-# LANGUAGE OverloadedStrings #-}
module Language.C.Ekisoba.AST ( Object(..) ) where

import qualified Data.Text as T

data Object = Object {
    name :: T.Text
} deriving (Eq, Show)

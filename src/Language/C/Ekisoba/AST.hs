{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.C.Ekisoba.AST ( Object(..) ) where

import qualified Data.Aeson    as Aes
import qualified Data.Aeson.TH as TH
import qualified Data.Text     as T

data Object = Object {
    name :: T.Text
} deriving (Eq, Show)

$(TH.deriveJSON TH.defaultOptions ''Object)

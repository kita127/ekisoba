{-# LANGUAGE OverloadedStrings #-}
module Language.C.Ekisoba ( translate ) where

import qualified Language.C.Ekisoba.Ast as EAST
import qualified Language.C.Syntax.AST  as AST

translate :: AST.CTranslUnit -> String
translate ast = show EAST.Object{EAST.name = "file name"}

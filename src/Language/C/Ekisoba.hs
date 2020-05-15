module Language.C.Ekisoba ( translate ) where

import qualified Language.C.Syntax.AST as AST

translate :: AST.CTranslUnit -> String
translate _ = "result"

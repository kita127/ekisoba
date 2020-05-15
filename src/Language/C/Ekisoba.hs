{-# LANGUAGE OverloadedStrings #-}
module Language.C.Ekisoba ( translate ) where

import qualified Data.Text                as T
import qualified Language.C.Data.Node     as Node
import qualified Language.C.Data.Position as Pos
import qualified Language.C.Ekisoba.AST   as EAST
import qualified Language.C.Syntax.AST    as AST

-- | translate
--
translate :: AST.CTranslUnit -> EAST.Object
translate (AST.CTranslUnit _ nodeInfo) = EAST.Object { EAST.name = extractNodeName nodeInfo }

-- | extractNodeName
--
extractNodeName :: Node.NodeInfo -> T.Text
extractNodeName (Node.OnlyPos _ _)    = error "only position error"
extractNodeName (Node.NodeInfo p _ _) = T.pack $ Pos.posFile p

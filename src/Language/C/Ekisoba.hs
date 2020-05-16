{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.C.Ekisoba ( translate ) where

import qualified Data.Text                   as T
import qualified Language.C.Data.Ident       as ID
import qualified Language.C.Data.Node        as Node
import qualified Language.C.Data.Position    as Pos
import qualified Language.C.Ekisoba.AST      as EAST
import qualified Language.C.Syntax.AST       as AST
import qualified Language.C.Syntax.Constants as Const

-- | translate
--
translate :: AST.CTranslUnit -> EAST.Object
translate (AST.CTranslUnit xs nodeInfo) =
    EAST.Object {
      EAST.name = extractNodeName nodeInfo
    , EAST.program = extractProgram xs
    }

-- | extractNodeName
--
extractNodeName :: Node.NodeInfo -> T.Text
extractNodeName (Node.OnlyPos _ _)    = error "only position error"
extractNodeName (Node.NodeInfo p _ _) = T.pack $ Pos.posFile p

-- | extractProgram
--
extractProgram :: [AST.CExternalDeclaration Node.NodeInfo] -> EAST.Program
extractProgram xs =
    EAST.Program {
      EAST.statements = map extractStatement xs
    }

-- | extractStatement
--
extractStatement :: AST.CExternalDeclaration Node.NodeInfo -> EAST.Statement
extractStatement (AST.CDeclExt x  ) = extractDeclaration x
extractStatement (AST.CFDefExt x  ) = undefined
extractStatement (AST.CAsmExt  x a) = undefined

-- | extractDeclaration
--
extractDeclaration :: AST.CDeclaration Node.NodeInfo -> EAST.Statement
extractDeclaration (AST.CDecl xs ys a)       = extractVarDef xs ys
extractDeclaration (AST.CStaticAssert x y a) = undefined

-- | extractVarDef
--
extractVarDef ::
      [AST.CDeclarationSpecifier Node.NodeInfo]
   -> [(Maybe (AST.CDeclarator Node.NodeInfo)
      , Maybe (AST.CInitializer Node.NodeInfo)
      , Maybe (AST.CExpression Node.NodeInfo))
      ]
   -> EAST.Statement
extractVarDef xs ys =
    EAST.VariableDefinition {
      EAST.name  = extractVarName $ head ys
    , EAST.typ  = map extractVarType xs
    , EAST.value = extractInitValue $ head ys
    }

-- | extractInitValue
--
extractInitValue ::
     ( Maybe (AST.CDeclarator Node.NodeInfo)
     , Maybe (AST.CInitializer Node.NodeInfo)
     , Maybe (AST.CExpression Node.NodeInfo)
     )
  -> Maybe T.Text
extractInitValue (x, Nothing, z) = Nothing
extractInitValue (x, Just y, z)  = Just $ extractInitializer y

-- | extractInitializer
--
extractInitializer :: AST.CInitializer Node.NodeInfo -> T.Text
extractInitializer (AST.CInitExpr x a ) = extractInitExpression x
extractInitializer (AST.CInitList x a ) = undefined

-- | extractInitExpression
--
extractInitExpression :: AST.CExpression Node.NodeInfo -> T.Text
extractInitExpression (AST.CComma xs a)              = undefined
extractInitExpression (AST.CAssign x y z a)          = undefined
extractInitExpression (AST.CCond x y z a)            = undefined
extractInitExpression (AST.CBinary x y z a)          = undefined
extractInitExpression (AST.CCast x y a)              = undefined
extractInitExpression (AST.CUnary x y a)             = undefined
extractInitExpression (AST.CSizeofExpr x a)          = undefined
extractInitExpression (AST.CSizeofType x a)          = undefined
extractInitExpression (AST.CAlignofExpr x a)         = undefined
extractInitExpression (AST.CAlignofType x a)         = undefined
extractInitExpression (AST.CComplexReal x a)         = undefined
extractInitExpression (AST.CComplexImag x a)         = undefined
extractInitExpression (AST.CIndex x y a)             = undefined
extractInitExpression (AST.CCall x ys a)             = undefined
extractInitExpression (AST.CMember x ident bool a)   = undefined
extractInitExpression (AST.CVar ident a)             = undefined
extractInitExpression (AST.CConst x)                 = extractConstant x
extractInitExpression (AST.CCompoundLit x y a)       = undefined
extractInitExpression (AST.CGenericSelection x ys a) = undefined
extractInitExpression (AST.CStatExpr x a)            = undefined
extractInitExpression (AST.CLabAddrExpr ident a)     = undefined
extractInitExpression (AST.CBuiltinExpr x)           = undefined

-- | extractConstant
--
extractConstant :: AST.CConstant Node.NodeInfo -> T.Text
extractConstant (AST.CIntConst n a)   = T.pack . show . Const.getCInteger $ n
extractConstant (AST.CCharConst x a)  = undefined
extractConstant (AST.CFloatConst x a) = undefined
extractConstant (AST.CStrConst x a)   = undefined


-- | extractVarName
--
extractVarName ::
     ( Maybe (AST.CDeclarator Node.NodeInfo)
     , Maybe (AST.CInitializer Node.NodeInfo)
     , Maybe (AST.CExpression Node.NodeInfo)
     )
  -> T.Text
extractVarName (Just x, y, z) = extractVarNameDeclr x
extractVarName  _               = undefined

-- | extractVarNameDeclr
--
extractVarNameDeclr :: AST.CDeclarator Node.NodeInfo -> T.Text
extractVarNameDeclr (AST.CDeclr (Just ident) xs y zs a) = extractVarNameDeclrSpec ident
extractVarNameDeclr _ = undefined

-- | extractVarNameDeclrSpec
--
extractVarNameDeclrSpec :: ID.Ident -> T.Text
extractVarNameDeclrSpec (ID.Ident name n a) = T.pack name



-- | extractVarType
--
extractVarType :: AST.CDeclarationSpecifier Node.NodeInfo -> T.Text
extractVarType (AST.CStorageSpec x) = undefined
extractVarType (AST.CTypeSpec    x) = extractVarTypeSpec x
extractVarType (AST.CTypeQual    x) = undefined
extractVarType (AST.CFunSpec     x) = undefined
extractVarType (AST.CAlignSpec   x) = undefined

-- | extractVarTypeSpec
--
extractVarTypeSpec :: AST.CTypeSpecifier Node.NodeInfo -> T.Text
extractVarTypeSpec (AST.CVoidType a                   ) = undefined
extractVarTypeSpec (AST.CCharType a                   ) = undefined
extractVarTypeSpec (AST.CShortType a                  ) = undefined
extractVarTypeSpec (AST.CIntType a                    ) = "int"
extractVarTypeSpec (AST.CLongType a                   ) = undefined
extractVarTypeSpec (AST.CFloatType a                  ) = undefined
extractVarTypeSpec (AST.CDoubleType a                 ) = undefined
extractVarTypeSpec (AST.CSignedType a                 ) = undefined
extractVarTypeSpec (AST.CUnsigType a                  ) = undefined
extractVarTypeSpec (AST.CBoolType a                   ) = undefined
extractVarTypeSpec (AST.CComplexType a                ) = undefined
extractVarTypeSpec (AST.CInt128Type a                 ) = undefined
extractVarTypeSpec (AST.CFloatNType int bool a        ) = undefined
extractVarTypeSpec (AST.CSUType x a )                   = undefined
extractVarTypeSpec (AST.CEnumType x a  )                = undefined
extractVarTypeSpec (AST.CTypeDef ident a              ) = undefined
extractVarTypeSpec (AST.CTypeOfExpr x a )               = undefined
extractVarTypeSpec (AST.CTypeOfType x a)                = undefined
extractVarTypeSpec (AST.CAtomicType x a)                = undefined

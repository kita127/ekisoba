{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.C.Ekisoba
(
  parseCFile
, Preprocessor(..)
, translate
, ParseError(..)
) where

import           Control.Monad
import qualified Control.Monad.Extra         as Ext
import qualified Data.Text                   as T
import qualified Language.C                  as C
import qualified Language.C.Data.Ident       as ID
import qualified Language.C.Data.Node        as Node
import qualified Language.C.Data.Position    as Pos
import qualified Language.C.Ekisoba.AST      as EAST
import qualified Language.C.Parser           as Pars
import qualified Language.C.Syntax.AST       as AST
import qualified Language.C.Syntax.Constants as Const
import qualified Language.C.System.GCC       as GCC

newtype ParseError = ParseError {
                     message :: String
                   }
data Preprocessor = GCC

-- | newParseError
--
newParseError :: String -> ParseError
newParseError s = ParseError { message = s }

-- | failParse
--
failParse :: String -> Either ParseError a
failParse = Left . newParseError

-- | preprocess (if necessary) and parse a C source file
--
--   > Synopsis: parseCFile preprocesssor cpp-opts file
--   > Example:  parseCFile GCC ["-I/usr/include/gtk-2.0"] my-gtk-exts.c
parseCFile :: Preprocessor -> [String] -> FilePath -> IO (Either ParseError EAST.Object)
parseCFile GCC opts file = do
    r <- C.parseCFile (GCC.newGCC "gcc") Nothing opts file
    case r of
        Right r' -> return . translate $ r'
        Left r'  -> return . failParse $ "language-c parse error"


-- | translate
--
translate :: AST.CTranslUnit -> Either ParseError EAST.Object
translate (AST.CTranslUnit xs nodeInfo) = do
    n <- extractNodeName nodeInfo
    p <- extractProgram xs
    return EAST.Object {
      EAST.name = n
    , EAST.program = p
    }

-- | extractNodeName
--
extractNodeName :: Node.NodeInfo -> Either ParseError T.Text
extractNodeName (Node.OnlyPos _ _)    = failParse "Couldn't extract node name"
extractNodeName (Node.NodeInfo p _ _) = Right . T.pack . Pos.posFile $ p

-- | extractProgram
--
extractProgram :: [AST.CExternalDeclaration Node.NodeInfo] -> Either ParseError EAST.Program
extractProgram xs = do
    ss <- Ext.concatMapM extractStatement xs
    return EAST.Program {
      EAST.statements = ss
    }

-- | extractStatement
--
extractStatement :: AST.CExternalDeclaration Node.NodeInfo -> Either ParseError [EAST.Statement]
extractStatement (AST.CDeclExt x  ) = extractDeclaration x
extractStatement (AST.CFDefExt x  ) = failParse "unimplemented"
extractStatement (AST.CAsmExt  x a) = failParse "unimplemented"

-- | extractDeclaration
--
extractDeclaration :: AST.CDeclaration Node.NodeInfo -> Either ParseError [EAST.Statement]
extractDeclaration (AST.CDecl xs ys a)       = extractVarDef xs ys
extractDeclaration (AST.CStaticAssert x y a) = failParse "unimplemented"

-- | extractVarDef
--
extractVarDef ::
      [AST.CDeclarationSpecifier Node.NodeInfo]
   -> [(Maybe (AST.CDeclarator Node.NodeInfo)
      , Maybe (AST.CInitializer Node.NodeInfo)
      , Maybe (AST.CExpression Node.NodeInfo))
      ]
   -> Either ParseError [EAST.Statement]
extractVarDef xs ys = do
    mapM (\y -> do
        n <- extractVarName y
        t <- mapM extractVarType xs
        v <- extractInitValue y
        return EAST.VariableDefinition {
                EAST.name = n
              , EAST.typ = t
              , EAST.value = v
            }
        ) ys

-- | extractInitValue
--
extractInitValue ::
     ( Maybe (AST.CDeclarator Node.NodeInfo)
     , Maybe (AST.CInitializer Node.NodeInfo)
     , Maybe (AST.CExpression Node.NodeInfo)
     )
  -> Either ParseError (Maybe T.Text)
extractInitValue (x, Nothing, z) = return Nothing
extractInitValue (x, Just y, z)  = return . Just $ extractInitializer y

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
extractConstant (AST.CCharConst x a)  = extractChar $ x
extractConstant (AST.CFloatConst x a) = undefined
extractConstant (AST.CStrConst x a)   = undefined

-- | extractChar
--
extractChar :: Const.CChar -> T.Text
extractChar (Const.CChar c b)   = T.pack . show $ c
extractChar (Const.CChars cs b) = undefined


-- | extractVarName
--
extractVarName ::
     ( Maybe (AST.CDeclarator Node.NodeInfo)
     , Maybe (AST.CInitializer Node.NodeInfo)
     , Maybe (AST.CExpression Node.NodeInfo)
     )
  -> Either ParseError T.Text
extractVarName (Just x, y, z) = extractVarNameDeclr x
extractVarName  _             = failParse "error extractVarName"

-- | extractVarNameDeclr
--
extractVarNameDeclr :: AST.CDeclarator Node.NodeInfo -> Either ParseError T.Text
extractVarNameDeclr (AST.CDeclr (Just ident) xs y zs a) = extractVarNameDeclrSpec ident
extractVarNameDeclr _ = failParse "error extractVarNameDeclr"

-- | extractVarNameDeclrSpec
--
extractVarNameDeclrSpec :: ID.Ident -> Either ParseError T.Text
extractVarNameDeclrSpec (ID.Ident name n a) = return . T.pack $ name



-- | extractVarType
--
extractVarType :: AST.CDeclarationSpecifier Node.NodeInfo -> Either ParseError T.Text
extractVarType (AST.CStorageSpec x) = extractStorageSpecifier x
extractVarType (AST.CTypeSpec    x) = extractVarTypeSpec x
extractVarType (AST.CTypeQual    x) = failParse "error extractVarType pattern match CTypeQual"
extractVarType (AST.CFunSpec     x) = failParse "error extractVarType pattern match CFunSpec"
extractVarType (AST.CAlignSpec   x) = failParse "error extractVarType pattern match CLignSpec"

-- | extractStorageSpecifier
--
extractStorageSpecifier :: AST.CStorageSpecifier Node.NodeInfo -> Either ParseError T.Text
extractStorageSpecifier (AST.CAuto a    ) = failParse "error extractStorageSpecifier match CAuto"
extractStorageSpecifier (AST.CRegister a) = failParse "error extractStorageSpecifier match CRegister"
extractStorageSpecifier (AST.CStatic a  ) = return "static"
extractStorageSpecifier (AST.CExtern a  ) = failParse "error extractStorageSpecifier match CExtern"
extractStorageSpecifier (AST.CTypedef a ) = failParse "error extractStorageSpecifier match CTypedef"
extractStorageSpecifier (AST.CThread a  ) = failParse "error extractStorageSpecifier match CThread"
extractStorageSpecifier (AST.CClKernel a) = failParse "error extractStorageSpecifier match CClKernel"
extractStorageSpecifier (AST.CClGlobal a) = failParse "error extractStorageSpecifier match CClGlobal"
extractStorageSpecifier (AST.CClLocal a ) = failParse "error extractStorageSpecifier match CClLocal"


-- | extractVarTypeSpec
--
extractVarTypeSpec :: AST.CTypeSpecifier Node.NodeInfo -> Either ParseError T.Text
extractVarTypeSpec (AST.CVoidType a                   ) = return "void"
extractVarTypeSpec (AST.CCharType a                   ) = return "char"
extractVarTypeSpec (AST.CShortType a                  ) = return "short"
extractVarTypeSpec (AST.CIntType a                    ) = return "int"
extractVarTypeSpec (AST.CLongType a                   ) = return "long"
extractVarTypeSpec (AST.CFloatType a                  ) = return "float"
extractVarTypeSpec (AST.CDoubleType a                 ) = return "double"
extractVarTypeSpec (AST.CSignedType a                 ) = return "signed"
extractVarTypeSpec (AST.CUnsigType a                  ) = return "unsigned"
extractVarTypeSpec (AST.CBoolType a                   ) = failParse "unimplemented BoolType"
extractVarTypeSpec (AST.CComplexType a                ) = failParse "unimplemented CComplexType"
extractVarTypeSpec (AST.CInt128Type a                 ) = failParse "unimplemented CInt128Type"
extractVarTypeSpec (AST.CFloatNType int bool a        ) = failParse "unimplemented CFloatNType"
extractVarTypeSpec (AST.CSUType x a )                   = failParse "unimplemented CSUType"
extractVarTypeSpec (AST.CEnumType x a  )                = failParse "unimplemented CEnumType"
extractVarTypeSpec (AST.CTypeDef ident a              ) = failParse "unimplemented CTypeDef"
extractVarTypeSpec (AST.CTypeOfExpr x a )               = failParse "unimplemented CTypeOfExpr"
extractVarTypeSpec (AST.CTypeOfType x a)                = failParse "unimplemented CTypeOfType"
extractVarTypeSpec (AST.CAtomicType x a)                = failParse "unimplemented CAtomicType"

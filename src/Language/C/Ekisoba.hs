module Language.C.Ekisoba
    ( parseCFile
    , Preprocessor(..)
    , translate
    , ParseError(..)
    )
where

import           Control.Monad
import qualified Control.Monad.Extra           as Ext
import qualified Data.Text                     as T
import qualified Language.C                    as C
import qualified Language.C.Data.Ident         as ID
import qualified Language.C.Data.Node          as Node
import qualified Language.C.Data.Position      as Pos
import qualified Language.C.Ekisoba.AST        as EAST
import qualified Language.C.Parser             as Pars
import qualified Language.C.Syntax.AST         as AST
import qualified Language.C.Syntax.Constants   as Const
import qualified Language.C.System.GCC         as GCC

newtype ParseError = ParseError {
                     message :: String
                   }
type EkiParser a = Either ParseError a

data Preprocessor = GCC

data Grammer = StructureDecl | VarDef | Typedef

-- | newParseError
--
newParseError :: String -> ParseError
newParseError s = ParseError { message = s }

-- | failParse
--
failParse :: String -> EkiParser a
failParse = Left . newParseError

-- | preprocess (if necessary) and parse a C source file
--
--   > Synopsis: parseCFile preprocesssor cpp-opts file
--   > Example:  parseCFile GCC ["-I/usr/include/gtk-2.0"] my-gtk-exts.c
parseCFile :: Preprocessor -> [String] -> FilePath -> IO (EkiParser EAST.Object)
parseCFile GCC opts file = do
    r <- C.parseCFile (GCC.newGCC "gcc") Nothing opts file
    case r of
        Right r' -> return . translate $ r'
        Left  r' -> return . failParse $ "language-c parse error"


-- | translate
--
translate :: AST.CTranslUnit -> EkiParser EAST.Object
translate (AST.CTranslUnit xs nodeInfo) = do
    n <- extractNodeName nodeInfo
    p <- extractProgram xs
    return EAST.Object { EAST.name = n, EAST.program = p }

-- | extractNodeName
--
extractNodeName :: Node.NodeInfo -> EkiParser T.Text
extractNodeName (Node.OnlyPos _ _   ) = failParse "Couldn't extract node name"
extractNodeName (Node.NodeInfo p _ _) = Right . T.pack . Pos.posFile $ p

-- | extractProgram
--
extractProgram
    :: [AST.CExternalDeclaration Node.NodeInfo] -> EkiParser EAST.Program
extractProgram xs = do
    ss <- Ext.concatMapM extractStatementList xs
    return EAST.Program { EAST.statements = ss }

-- | extractStatementList
--
extractStatementList
    :: AST.CExternalDeclaration Node.NodeInfo -> EkiParser [EAST.Statement]
extractStatementList (AST.CDeclExt x ) = extractDeclaration x
extractStatementList (AST.CFDefExt x ) = extractCFunctionDef x
extractStatementList (AST.CAsmExt x a) = failParse "unimplemented CAsmExt"

-- | extractCFunctionDef
--
extractCFunctionDef
    :: AST.CFunctionDef Node.NodeInfo -> EkiParser [EAST.Statement]
extractCFunctionDef (AST.CFunDef xs y zs q a) = do
    ts   <- mapM extractVarType xs
    n    <- extractVarNameDeclr y
    args <- extractArgs y
    body <- extractStatement q
    return
        [ EAST.FunctionDefinition { EAST.name    = n
                                  , EAST.retType = ts
                                  , EAST.args    = args
                                  , EAST.body    = body
                                  }
        ]

-- | extractCompBlockItem
--
extractCompBlockItem
    :: AST.CCompoundBlockItem Node.NodeInfo -> EkiParser [EAST.Statement]
extractCompBlockItem (AST.CBlockStmt x) = (: []) <$> extractStatement x
extractCompBlockItem (AST.CBlockDecl x) = extractDeclaration x
extractCompBlockItem (AST.CNestedFunDef x) =
    failParse "unimplemented extractCompBlockItem 2"

-- | extractStatement
--
extractStatement :: AST.CStatement Node.NodeInfo -> EkiParser EAST.Statement
extractStatement (AST.CLabel ident x ys a) =
    failParse "unimplemented extractStatement 1"
extractStatement (AST.CCase value statement a) =
    EAST.CaseStatement
        <$> extractExpression value
        <*> extractStatement statement
extractStatement (AST.CCases x y z a) =
    failParse "unimplemented extractStatement 3"
extractStatement (AST.CDefault statement a) =
    EAST.DefaultStatement <$> extractStatement statement
extractStatement (AST.CExpr (Just x) a) =
    EAST.ExpressionStatement <$> extractExpression x
extractStatement (AST.CCompound idents xs a) =
    EAST.BlockStatement <$> Ext.concatMapM extractCompBlockItem xs
extractStatement (AST.CIf condition consequence Nothing a) =
    EAST.IfStatement
        <$> extractExpression condition
        <*> extractStatement consequence
        <*> pure Nothing

extractStatement (AST.CIf condition consequence (Just alternative) a) =
    EAST.IfStatement
        <$> extractExpression condition
        <*> extractStatement consequence
        <*> (Just <$> extractStatement alternative)

extractStatement (AST.CSwitch condition cases a) =
    EAST.SwitchStatement
        <$> extractExpression condition
        <*> extractStatement cases
extractStatement (AST.CWhile condition stm bool a) =
    EAST.WhileStatement <$> extractExpression condition <*> extractStatement stm
extractStatement (AST.CFor x y z q a) =
    failParse "unimplemented extractStatement 10"
extractStatement (AST.CGoto ident a) =
    failParse "unimplemented extractStatement 11"
extractStatement (AST.CGotoPtr x a) =
    failParse "unimplemented extractStatement 12"
extractStatement (AST.CCont  a) = failParse "unimplemented extractStatement 13"
extractStatement (AST.CBreak a) = pure EAST.Break
extractStatement (AST.CReturn (Just x) a) =
    EAST.ReturnStatement <$> (Just <$> extractExpression x)
extractStatement (AST.CAsm x a) = failParse "unimplemented extractStatement 16"
extractStatement _              = failParse "unimplemented extractStatement 99"


-- | extractArgs
--
extractArgs :: AST.CDeclarator Node.NodeInfo -> EkiParser EAST.Statement
extractArgs (AST.CDeclr ident xs y zs a) =
    EAST.Argument <$> extractArgsDerivedDec xs

-- | extractArgsDerivedDec
--
extractArgsDerivedDec
    :: [AST.CDerivedDeclarator Node.NodeInfo] -> EkiParser [EAST.Statement]
extractArgsDerivedDec ((AST.CPtrDeclr xs a) : _) =
    failParse "unimplemented extractArgsDerivedDec 1"
extractArgsDerivedDec ((AST.CArrDeclr xs y a) : _) =
    failParse "unimplemented extractArgsDerivedDec 2"
extractArgsDerivedDec ((AST.CFunDeclr (Right (xs, bool)) ys a) : _) =
    Ext.concatMapM extractDeclaration xs
extractArgsDerivedDec _ = undefined

-- | extractDeclaration
--
extractDeclaration
    :: AST.CDeclaration Node.NodeInfo -> EkiParser [EAST.Statement]
extractDeclaration (AST.CDecl xs ys a) = extractVarDefOrStructure xs ys
extractDeclaration (AST.CStaticAssert x y a) =
    failParse "unimplemented extractDec"

-- | extractVarDefOrStructure
--
extractVarDefOrStructure
    :: [AST.CDeclarationSpecifier Node.NodeInfo]
    -> [ ( Maybe (AST.CDeclarator Node.NodeInfo)
         , Maybe (AST.CInitializer Node.NodeInfo)
         , Maybe (AST.CExpression Node.NodeInfo)
         )
       ]
    -> EkiParser [EAST.Statement]
extractVarDefOrStructure xs ys = case searchGrammer (head xs) ys of
    StructureDecl -> (: []) <$> extractStrUniDecl (head xs)
    Typedef       -> extractTypedef xs ys
    VarDef        -> extractVarDef xs ys

-- | extractTypedef
--
extractTypedef
    :: [AST.CDeclarationSpecifier Node.NodeInfo]
    -> [ ( Maybe (AST.CDeclarator Node.NodeInfo)
         , Maybe (AST.CInitializer Node.NodeInfo)
         , Maybe (AST.CExpression Node.NodeInfo)
         )
       ]
    -> EkiParser [EAST.Statement]
extractTypedef xs@((AST.CStorageSpec (AST.CTypedef a)) : _) (y : ys) = do
    n  <- extractVarName y
    ts <- mapM extractVarType xs
    -- Discarded beginning because no matter
    return [EAST.TypdefDeclaration { EAST.name = n, EAST.typ = tail ts }]
extractTypedef _ _ = failParse "error extractTypedef"



-- | extractStrUniDecl
--
extractStrUniDecl
    :: AST.CDeclarationSpecifier Node.NodeInfo -> EkiParser EAST.Statement
extractStrUniDecl (AST.CTypeSpec x) = extractVarTypeSpec x


-- | searchGrammer
--
searchGrammer :: AST.CDeclarationSpecifier Node.NodeInfo -> [a] -> Grammer
searchGrammer (AST.CTypeSpec (AST.CSUType _ a)) [] = StructureDecl
searchGrammer (AST.CStorageSpec (AST.CTypedef a)) _ = Typedef
searchGrammer _ _  = VarDef

-- | extractVarDef
--
extractVarDef
    :: [AST.CDeclarationSpecifier Node.NodeInfo]
    -> [ ( Maybe (AST.CDeclarator Node.NodeInfo)
         , Maybe (AST.CInitializer Node.NodeInfo)
         , Maybe (AST.CExpression Node.NodeInfo)
         )
       ]
    -> EkiParser [EAST.Statement]
extractVarDef xs ys = do
    ts <- mapM extractVarType xs
    if isVoid (head ts)
        then return
            [ EAST.VariableDefinition { EAST.name  = ""
                                      , EAST.typ   = ts
                                      , EAST.value = Nothing
                                      }
            ]
        else mapM
            (\y -> do
                n  <- extractVarName y
                ps <- Ext.concatMapM extractPointer ys
                v  <- extractInitValue y
                return EAST.VariableDefinition { EAST.name  = n
                                               , EAST.typ   = ts ++ ps
                                               , EAST.value = v
                                               }
            )
            ys

-- | isVoid
--
isVoid :: EAST.Statement -> Bool
isVoid EAST.Type { name = n } = n == "void"
isVoid _                      = False

-- | extractPointer
--
extractPointer
    :: ( Maybe (AST.CDeclarator Node.NodeInfo)
       , Maybe (AST.CInitializer Node.NodeInfo)
       , Maybe (AST.CExpression Node.NodeInfo)
       )
    -> EkiParser [EAST.Statement]
extractPointer (Just x, y, z) = extractPointerCDeclarator x
extractPointer _              = failParse "error extractPointer"

-- | extractPointerCDeclarator
--
extractPointerCDeclarator :: AST.CDeclarator a -> EkiParser [EAST.Statement]
extractPointerCDeclarator (AST.CDeclr ident xs y zs a) =
    mapM extractPointerCDerivedDeclarator xs

-- | extractPointerCDerivedDeclarator
--
extractPointerCDerivedDeclarator
    :: AST.CDerivedDeclarator a -> EkiParser EAST.Statement
extractPointerCDerivedDeclarator (AST.CPtrDeclr xs a) = return $ newType "*"
extractPointerCDerivedDeclarator (AST.CArrDeclr xs y a) =
    failParse "error CArrDeclr"
extractPointerCDerivedDeclarator (AST.CFunDeclr x ys a) =
    failParse "error CFunDeclr"


-- | extractInitValue
--
extractInitValue
    :: ( Maybe (AST.CDeclarator Node.NodeInfo)
       , Maybe (AST.CInitializer Node.NodeInfo)
       , Maybe (AST.CExpression Node.NodeInfo)
       )
    -> EkiParser (Maybe EAST.Expression)
extractInitValue (x, Nothing, z) = return Nothing
extractInitValue (x, Just y , z) = Just <$> extractInitializer y

-- | extractInitializer
--
extractInitializer
    :: AST.CInitializer Node.NodeInfo -> EkiParser EAST.Expression
extractInitializer (AST.CInitExpr x a) = extractExpression x
extractInitializer (AST.CInitList x a) = undefined

-- | extractExpression
--
extractExpression :: AST.CExpression Node.NodeInfo -> EkiParser EAST.Expression
extractExpression (AST.CComma xs a            ) = undefined
extractExpression (AST.CAssign op left right a) = extractAssignOp op left right
extractExpression (AST.CCond   x  y    z     a) = undefined
extractExpression (AST.CBinary op left right a) = extractBinaryOp op left right
extractExpression (AST.CCast  x y a           ) = undefined
extractExpression (AST.CUnary x y a           ) = undefined
extractExpression (AST.CSizeofExpr  x a       ) = undefined
extractExpression (AST.CSizeofType  x a       ) = undefined
extractExpression (AST.CAlignofExpr x a       ) = undefined
extractExpression (AST.CAlignofType x a       ) = undefined
extractExpression (AST.CComplexReal x a       ) = undefined
extractExpression (AST.CComplexImag x a       ) = undefined
extractExpression (AST.CIndex x y  a          ) = undefined
extractExpression (AST.CCall  x ys a          ) = undefined
extractExpression (AST.CMember x ident bool a ) = undefined
extractExpression (AST.CVar ident a           ) = do
    n <- extractVarNameDeclrSpec ident
    return EAST.Identifire { EAST.name = n }
extractExpression (AST.CConst x                ) = extractConstant x
extractExpression (AST.CCompoundLit      x y  a) = undefined
extractExpression (AST.CGenericSelection x ys a) = undefined
extractExpression (AST.CStatExpr    x     a    ) = undefined
extractExpression (AST.CLabAddrExpr ident a    ) = undefined
extractExpression (AST.CBuiltinExpr x          ) = undefined

-- | extractAssignOp
--
extractAssignOp
    :: AST.CAssignOp
    -> AST.CExpression Node.NodeInfo
    -> AST.CExpression Node.NodeInfo
    -> EkiParser EAST.Expression
extractAssignOp AST.CAssignOp left right = newInfixExpression left "=" right
extractAssignOp AST.CMulAssOp left right = undefined
extractAssignOp AST.CDivAssOp left right = undefined
extractAssignOp AST.CRmdAssOp left right = undefined
extractAssignOp AST.CAddAssOp left right = undefined
extractAssignOp AST.CSubAssOp left right = undefined
extractAssignOp AST.CShlAssOp left right = undefined
extractAssignOp AST.CShrAssOp left right = undefined
extractAssignOp AST.CAndAssOp left right = undefined
extractAssignOp AST.CXorAssOp left right = undefined
extractAssignOp AST.COrAssOp  left right = undefined



-- | extractBinaryOp
--
extractBinaryOp
    :: AST.CBinaryOp
    -> AST.CExpression Node.NodeInfo
    -> AST.CExpression Node.NodeInfo
    -> EkiParser EAST.Expression
extractBinaryOp AST.CMulOp left right = newInfixExpression left "*" right
extractBinaryOp AST.CDivOp left right = newInfixExpression left "/" right
extractBinaryOp AST.CRmdOp left right = failParse "Rmd Op"
extractBinaryOp AST.CAddOp left right = newInfixExpression left "+" right
extractBinaryOp AST.CSubOp left right = newInfixExpression left "-" right
extractBinaryOp AST.CShlOp left right = failParse "Shl Op"
extractBinaryOp AST.CShrOp left right = failParse "Shr Op"
extractBinaryOp AST.CLeOp  left right = failParse "Le Op"
extractBinaryOp AST.CGrOp  left right = newInfixExpression left ">" right
extractBinaryOp AST.CLeqOp left right = failParse "Leq Op"
extractBinaryOp AST.CGeqOp left right = newInfixExpression left ">=" right
extractBinaryOp AST.CEqOp  left right = newInfixExpression left "==" right
extractBinaryOp AST.CNeqOp left right = failParse "Neq Op"
extractBinaryOp AST.CAndOp left right = failParse "And Op"
extractBinaryOp AST.CXorOp left right = failParse "Xor Op"
extractBinaryOp AST.COrOp  left right = failParse "Or Op"
extractBinaryOp AST.CLndOp left right = failParse "Lnd Op"
extractBinaryOp AST.CLorOp left right = failParse "Lor Op"

-- | newInfixExpression
--
newInfixExpression
    :: AST.CExpression Node.NodeInfo
    -> T.Text
    -> AST.CExpression Node.NodeInfo
    -> EkiParser EAST.Expression
newInfixExpression left op right =
    EAST.InfixExpression
        <$> extractExpression left
        <*> pure op
        <*> extractExpression right

-- | extractConstant
--
extractConstant :: AST.CConstant Node.NodeInfo -> EkiParser EAST.Expression
extractConstant (AST.CIntConst n a) =
    return EAST.IntegerLiteral { EAST.intVal = Const.getCInteger n }
extractConstant (AST.CCharConst  x a) = extractChar x
extractConstant (AST.CFloatConst x a) = undefined
extractConstant (AST.CStrConst   x a) = undefined

-- | extractChar
--
extractChar :: Const.CChar -> EkiParser EAST.Expression
extractChar (Const.CChar  c  b) = return EAST.CharLiteral { EAST.charVal = c }
extractChar (Const.CChars cs b) = undefined


-- | extractVarName
--
extractVarName
    :: ( Maybe (AST.CDeclarator Node.NodeInfo)
       , Maybe (AST.CInitializer Node.NodeInfo)
       , Maybe (AST.CExpression Node.NodeInfo)
       )
    -> EkiParser T.Text
extractVarName (Just x, y, z) = extractVarNameDeclr x
extractVarName _              = failParse "error extractVarName"

-- | extractVarNameDeclr
--
extractVarNameDeclr :: AST.CDeclarator Node.NodeInfo -> EkiParser T.Text
extractVarNameDeclr (AST.CDeclr (Just ident) xs y zs a) =
    extractVarNameDeclrSpec ident
extractVarNameDeclr _ = failParse "error extractVarNameDeclr"

-- | extractVarNameDeclrSpec
--
extractVarNameDeclrSpec :: ID.Ident -> EkiParser T.Text
extractVarNameDeclrSpec (ID.Ident name n a) = return . T.pack $ name



-- | extractVarType
--
extractVarType
    :: AST.CDeclarationSpecifier Node.NodeInfo -> EkiParser EAST.Statement
extractVarType (AST.CStorageSpec x) = extractStorageSpecifier x
extractVarType (AST.CTypeSpec    x) = extractVarTypeSpec x
extractVarType (AST.CTypeQual    x) = extractTypeQualifier x
extractVarType (AST.CFunSpec x) =
    failParse "error extractVarType pattern match CFunSpec"
extractVarType (AST.CAlignSpec x) =
    failParse "error extractVarType pattern match CLignSpec"

-- | extractTypeQualifier
--
extractTypeQualifier :: AST.CTypeQualifier a -> EkiParser EAST.Statement
extractTypeQualifier (AST.CConstQual    a) = return $ newType "const"
extractTypeQualifier (AST.CVolatQual    a) = return $ newType "volatile"
extractTypeQualifier (AST.CRestrQual    a) = failParse "CRestrQual"
extractTypeQualifier (AST.CAtomicQual   a) = failParse "CAtomicQual"
extractTypeQualifier (AST.CAttrQual     x) = failParse "CAttrQual"
extractTypeQualifier (AST.CNullableQual a) = failParse "CNullableQual"
extractTypeQualifier (AST.CNonnullQual  a) = failParse "CNonnullQual"
extractTypeQualifier (AST.CClRdOnlyQual a) = failParse "CClRdOnlyQual"
extractTypeQualifier (AST.CClWrOnlyQual a) = failParse "CClWrOnlyQual"


-- | extractStorageSpecifier
--
extractStorageSpecifier
    :: AST.CStorageSpecifier Node.NodeInfo -> EkiParser EAST.Statement
extractStorageSpecifier (AST.CAuto a) =
    failParse "error extractStorageSpecifier match CAuto"
extractStorageSpecifier (AST.CRegister a) =
    failParse "error extractStorageSpecifier match CRegister"
extractStorageSpecifier (AST.CStatic a) = return $ newType "static"
extractStorageSpecifier (AST.CExtern a) =
    failParse "error extractStorageSpecifier match CExtern"
extractStorageSpecifier (AST.CTypedef a) =
    return $ EAST.ASTStmtInfo "typdef qualifier"
extractStorageSpecifier (AST.CThread a) =
    failParse "error extractStorageSpecifier match CThread"
extractStorageSpecifier (AST.CClKernel a) =
    failParse "error extractStorageSpecifier match CClKernel"
extractStorageSpecifier (AST.CClGlobal a) =
    failParse "error extractStorageSpecifier match CClGlobal"
extractStorageSpecifier (AST.CClLocal a) =
    failParse "error extractStorageSpecifier match CClLocal"


-- | extractVarTypeSpec
--
extractVarTypeSpec
    :: AST.CTypeSpecifier Node.NodeInfo -> EkiParser EAST.Statement
extractVarTypeSpec (AST.CVoidType   a) = return $ newType "void"
extractVarTypeSpec (AST.CCharType   a) = return $ newType "char"
extractVarTypeSpec (AST.CShortType  a) = return $ newType "short"
extractVarTypeSpec (AST.CIntType    a) = return $ newType "int"
extractVarTypeSpec (AST.CLongType   a) = return $ newType "long"
extractVarTypeSpec (AST.CFloatType  a) = return $ newType "float"
extractVarTypeSpec (AST.CDoubleType a) = return $ newType "double"
extractVarTypeSpec (AST.CSignedType a) = return $ newType "signed"
extractVarTypeSpec (AST.CUnsigType  a) = return $ newType "unsigned"
extractVarTypeSpec (AST.CBoolType   a) = failParse "unimplemented BoolType"
extractVarTypeSpec (AST.CComplexType a) =
    failParse "unimplemented CComplexType"
extractVarTypeSpec (AST.CInt128Type a) = failParse "unimplemented CInt128Type"
extractVarTypeSpec (AST.CFloatNType int bool a) =
    failParse "unimplemented CFloatNType"
extractVarTypeSpec (AST.CSUType   x a) = extractStructureUnion x
extractVarTypeSpec (AST.CEnumType x a) = failParse "unimplemented CEnumType"
extractVarTypeSpec (AST.CTypeDef ident a) =
    newType <$> extractVarNameDeclrSpec ident
extractVarTypeSpec (AST.CTypeOfExpr x a) =
    failParse "unimplemented CTypeOfExpr"
extractVarTypeSpec (AST.CTypeOfType x a) =
    failParse "unimplemented CTypeOfType"
extractVarTypeSpec (AST.CAtomicType x a) =
    failParse "unimplemented CAtomicType"

-- | newType
--
newType :: T.Text -> EAST.Statement
newType n = EAST.Type { name = n }

-- | extractStructureUnion
--
extractStructureUnion
    :: AST.CStructureUnion Node.NodeInfo -> EkiParser EAST.Statement
extractStructureUnion (AST.CStruct AST.CStructTag (Just ident) (Just xs) ys a)
    = do
        n  <- extractVarNameDeclrSpec ident
        ms <- Ext.concatMapM extractDeclaration xs
        return EAST.StructDeclaration { EAST.name = n, EAST.menbers = ms }
extractStructureUnion (AST.CStruct AST.CStructTag (Just ident) Nothing [] a) =
    do
        n <- extractVarNameDeclrSpec ident
        return . newType $ "struct " <> n
extractStructureUnion (AST.CStruct AST.CUnionTag ident x ys a) =
    failParse "CUnionTag"
extractStructureUnion _ = failParse "StructureUnion error"

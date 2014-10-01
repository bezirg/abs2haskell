

module AbsABS where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq,Ord,Show,Read)
newtype TypeIdent = TypeIdent String deriving (Eq,Ord,Show,Read)
data Type =
   TyUnit
 | TyInt
 | TyRat
 | TyFut Type
 | TyUnderscore
 | TypeVar QualType
 | ArgType QualType [AnnType]
  deriving (Eq,Ord,Show,Read)

data AnnType =
   AnnType [Ann] Type
  deriving (Eq,Ord,Show,Read)

data QualType =
   QualType [QualTypeIdent]
  deriving (Eq,Ord,Show,Read)

data QualTypeIdent =
   QualTypeIdent TypeIdent
  deriving (Eq,Ord,Show,Read)

data Program =
   Prog ModuleDecl
  deriving (Eq,Ord,Show,Read)

data ModuleDecl =
   ModuleDecl QualType [Export] [Import] [AnnDecl] MaybeBlock
  deriving (Eq,Ord,Show,Read)

data Export =
   ExportAny [AnyIdent]
 | ExportAnyFrom [AnyIdent] QualType
 | ExportStar
 | ExportStarFrom QualType
  deriving (Eq,Ord,Show,Read)

data ImportType =
   ImportHaskell
 | ImportABS
  deriving (Eq,Ord,Show,Read)

data Import =
   ImportAnyFrom ImportType [AnyIdent] QualType
 | ImportStarFrom ImportType QualType
  deriving (Eq,Ord,Show,Read)

data AnyIdent =
   AnyIdentI Ident
 | AnyIdentT TypeIdent
  deriving (Eq,Ord,Show,Read)

data AnnDecl =
   AnnDecl [Ann] Decl
  deriving (Eq,Ord,Show,Read)

data Decl =
   TypeDecl TypeIdent Type
 | DataDecl TypeIdent [ConstrIdent]
 | ParDataDecl TypeIdent [TypeIdent] [ConstrIdent]
 | Fun Type Ident [Param] FunBody
 | ParFun Type Ident [TypeIdent] [Param] FunBody
 | InterfDecl TypeIdent [MethSig]
 | ExtendsDecl TypeIdent [QualType] [MethSig]
 | ClassDecl TypeIdent [BodyDecl] MaybeBlock [BodyDecl]
 | ClassParamDecl TypeIdent [Param] [BodyDecl] MaybeBlock [BodyDecl]
 | ClassImplements TypeIdent [QualType] [BodyDecl] MaybeBlock [BodyDecl]
 | ClassParamImplements TypeIdent [Param] [QualType] [BodyDecl] MaybeBlock [BodyDecl]
  deriving (Eq,Ord,Show,Read)

data ConstrIdent =
   UnaryConstr TypeIdent
 | MultConstr TypeIdent [ConstrType]
  deriving (Eq,Ord,Show,Read)

data ConstrType =
   EmptyConstrType Type
 | RecordConstrType Type Ident
  deriving (Eq,Ord,Show,Read)

data MethSig =
   MethSig Type Ident [Param]
  deriving (Eq,Ord,Show,Read)

data BodyDecl =
   FieldDecl Type Ident
 | FieldDeclAss Type Ident PureExp
 | MethDecl Type Ident [Param] Block
  deriving (Eq,Ord,Show,Read)

data Block =
   Block [Stm]
  deriving (Eq,Ord,Show,Read)

data MaybeBlock =
   JustBlock Block
 | NoBlock
  deriving (Eq,Ord,Show,Read)

data FunBody =
   Builtin
 | PureBody PureExp
  deriving (Eq,Ord,Show,Read)

data Param =
   Par AnnType Ident
  deriving (Eq,Ord,Show,Read)

data PureExp =
   Let Param PureExp PureExp
 | If PureExp PureExp PureExp
 | Case PureExp [CaseBranch]
 | EOr PureExp PureExp
 | EAnd PureExp PureExp
 | EEq PureExp PureExp
 | ENeq PureExp PureExp
 | ELt PureExp PureExp
 | ELe PureExp PureExp
 | EGt PureExp PureExp
 | EGe PureExp PureExp
 | EAdd PureExp PureExp
 | ESub PureExp PureExp
 | EMul PureExp PureExp
 | EDiv PureExp PureExp
 | EMod PureExp PureExp
 | ELogNeg PureExp
 | EIntNeg PureExp
 | ECall Ident [PureExp]
 | ENaryCall Ident [PureExp]
 | ELit Literal
 | EVar Ident
 | EThis Ident
 | EUnaryConstr QualType
 | EMultConstr QualType [PureExp]
  deriving (Eq,Ord,Show,Read)

data CaseBranch =
   CBranch Pattern PureExp
  deriving (Eq,Ord,Show,Read)

data Pattern =
   PIdent Ident
 | PLit Literal
 | PUnaryConstr TypeIdent
 | PMultConstr TypeIdent [Pattern]
 | PUnderscore
  deriving (Eq,Ord,Show,Read)

data Stm =
   SExp EffExp
 | SBlock [Stm]
 | SWhile PureExp Stm
 | SReturn Exp
 | SAss Ident Exp
 | SFieldAss Ident Exp
 | SDec Type Ident
 | SDecAss Type Ident Exp
 | SIf PureExp Stm
 | SIfElse PureExp Stm Stm
 | SAwait Guard
 | SSuspend
 | SSkip
 | SAssert PureExp
  deriving (Eq,Ord,Show,Read)

data Guard =
   VarGuard Ident
 | FieldGuard Ident
 | ExpGuard PureExp
 | AndGuard Guard Guard
  deriving (Eq,Ord,Show,Read)

data Literal =
   LNull
 | LThis
 | LThisDC
 | LStr String
 | LInt Integer
  deriving (Eq,Ord,Show,Read)

data EffExp =
   New Type [PureExp]
 | NewLocal Type [PureExp]
 | SyncCall PureExp Ident [PureExp]
 | ThisSyncCall Ident [PureExp]
 | AsyncCall PureExp Ident [PureExp]
 | ThisAsyncCall Ident [PureExp]
 | Get PureExp
 | Spawns PureExp Type [PureExp]
  deriving (Eq,Ord,Show,Read)

data Exp =
   ExpP PureExp
 | ExpE EffExp
  deriving (Eq,Ord,Show,Read)

data Ann =
   SimpleAnn PureExp
  deriving (Eq,Ord,Show,Read)


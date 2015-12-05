module Lang.ABS.Compiler.BNFC.SkelABS where

-- Haskell module generated by the BNF converter

import Lang.ABS.Compiler.BNFC.AbsABS
import Lang.ABS.Compiler.BNFC.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transUIdent :: UIdent -> Result
transUIdent x = case x of
  UIdent string -> failure x
transLIdent :: LIdent -> Result
transLIdent x = case x of
  LIdent string -> failure x
transAnyIdent :: AnyIdent -> Result
transAnyIdent x = case x of
  AnyIden lident -> failure x
  AnyTyIden uident -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog modules -> failure x
transModule :: Module -> Result
transModule x = case x of
  Modul qtype exports imports annotdecls maybeblock -> failure x
transExport :: Export -> Result
transExport x = case x of
  AnyExport anyidents -> failure x
  AnyFromExport anyidents qtype -> failure x
  StarExport -> failure x
  StarFromExport qtype -> failure x
transImport :: Import -> Result
transImport x = case x of
  AnyImport importtype ttype anyident -> failure x
  AnyFromImport importtype anyidents qtype -> failure x
  StarFromImport importtype qtype -> failure x
transImportType :: ImportType -> Result
transImportType x = case x of
  ForeignImport -> failure x
  NormalImport -> failure x
transType :: Type -> Result
transType x = case x of
  TUnderscore -> failure x
  TSimple qtype -> failure x
  TGen qtype types -> failure x
transQType :: QType -> Result
transQType x = case x of
  QTyp qtypesegments -> failure x
transQTypeSegment :: QTypeSegment -> Result
transQTypeSegment x = case x of
  QTypeSegmen uident -> failure x
transTType :: TType -> Result
transTType x = case x of
  TTyp ttypesegments -> failure x
transTTypeSegment :: TTypeSegment -> Result
transTTypeSegment x = case x of
  TTypeSegmen uident -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  TypeDecl uident type_ -> failure x
  TypeParDecl uident uidents type_ -> failure x
  ExceptionDecl constrident -> failure x
  DataDecl uident constridents -> failure x
  DataParDecl uident uidents constridents -> failure x
  FunDecl type_ lident params funbody -> failure x
  FunParDecl type_ lident uidents params funbody -> failure x
  InterfDecl uident annotmethsignats -> failure x
  ExtendsDecl uident qtypes annotmethsignats -> failure x
  ClassDecl uident classbodys1 maybeblock classbodys2 -> failure x
  ClassParamDecl uident params classbodys1 maybeblock classbodys2 -> failure x
  ClassImplements uident qtypes classbodys1 maybeblock classbodys2 -> failure x
  ClassParamImplements uident params qtypes classbodys1 maybeblock classbodys2 -> failure x
transConstrIdent :: ConstrIdent -> Result
transConstrIdent x = case x of
  SinglConstrIdent uident -> failure x
  ParamConstrIdent uident constrtypes -> failure x
transConstrType :: ConstrType -> Result
transConstrType x = case x of
  EmptyConstrType type_ -> failure x
  RecordConstrType type_ lident -> failure x
transFunBody :: FunBody -> Result
transFunBody x = case x of
  BuiltinFunBody -> failure x
  NormalFunBody pureexp -> failure x
transMethSignat :: MethSignat -> Result
transMethSignat x = case x of
  MethSig type_ lident params -> failure x
transClassBody :: ClassBody -> Result
transClassBody x = case x of
  FieldClassBody type_ lident -> failure x
  FieldAssignClassBody type_ lident pureexp -> failure x
  MethClassBody type_ lident params block -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Bloc annotstms -> failure x
transMaybeBlock :: MaybeBlock -> Result
transMaybeBlock x = case x of
  JustBlock annots block -> failure x
  NoBlock -> failure x
transParam :: Param -> Result
transParam x = case x of
  Par type_ lident -> failure x
transStm :: Stm -> Result
transStm x = case x of
  SExp exp -> failure x
  SBlock annotstms -> failure x
  SWhile pureexp annotstm -> failure x
  SReturn exp -> failure x
  SAss lident exp -> failure x
  SFieldAss lident exp -> failure x
  SDec type_ lident -> failure x
  SDecAss type_ lident exp -> failure x
  SIf pureexp annotstm -> failure x
  SIfElse pureexp annotstm1 annotstm2 -> failure x
  SSuspend -> failure x
  SSkip -> failure x
  SAssert pureexp -> failure x
  SAwait awaitguard -> failure x
  SThrow pureexp -> failure x
  SGive pureexp1 pureexp2 -> failure x
  STryCatchFinally annotstm catchbranchs maybefinally -> failure x
  SPrint pureexp -> failure x
transCatchBranch :: CatchBranch -> Result
transCatchBranch x = case x of
  CatchBranc pattern annotstm -> failure x
transMaybeFinally :: MaybeFinally -> Result
transMaybeFinally x = case x of
  JustFinally annotstm -> failure x
  NoFinally -> failure x
transAwaitGuard :: AwaitGuard -> Result
transAwaitGuard x = case x of
  FutGuard lident -> failure x
  ProGuard lident -> failure x
  FutFieldGuard lident -> failure x
  ProFieldGuard lident -> failure x
  ExpGuard pureexp -> failure x
  AndGuard awaitguard1 awaitguard2 -> failure x
transExp :: Exp -> Result
transExp x = case x of
  ExpP pureexp -> failure x
  ExpE effexp -> failure x
transPureExp :: PureExp -> Result
transPureExp x = case x of
  EOr pureexp1 pureexp2 -> failure x
  EAnd pureexp1 pureexp2 -> failure x
  EEq pureexp1 pureexp2 -> failure x
  ENeq pureexp1 pureexp2 -> failure x
  ELt pureexp1 pureexp2 -> failure x
  ELe pureexp1 pureexp2 -> failure x
  EGt pureexp1 pureexp2 -> failure x
  EGe pureexp1 pureexp2 -> failure x
  EAdd pureexp1 pureexp2 -> failure x
  ESub pureexp1 pureexp2 -> failure x
  EMul pureexp1 pureexp2 -> failure x
  EDiv pureexp1 pureexp2 -> failure x
  EMod pureexp1 pureexp2 -> failure x
  ELogNeg pureexp -> failure x
  EIntNeg pureexp -> failure x
  EFunCall lident pureexps -> failure x
  EQualFunCall ttype lident pureexps -> failure x
  ENaryFunCall lident pureexps -> failure x
  ENaryQualFunCall ttype lident pureexps -> failure x
  EVar lident -> failure x
  EThis lident -> failure x
  EQualVar ttype lident -> failure x
  ESinglConstr qtype -> failure x
  EParamConstr qtype pureexps -> failure x
  ELit literal -> failure x
  Let param pureexp1 pureexp2 -> failure x
  If pureexp1 pureexp2 pureexp3 -> failure x
  Case pureexp casebranchs -> failure x
transCaseBranch :: CaseBranch -> Result
transCaseBranch x = case x of
  CaseBranc pattern pureexp -> failure x
transPattern :: Pattern -> Result
transPattern x = case x of
  PIdent lident -> failure x
  PLit literal -> failure x
  PSinglConstr uident -> failure x
  PParamConstr uident patterns -> failure x
  PUnderscore -> failure x
transLiteral :: Literal -> Result
transLiteral x = case x of
  LNull -> failure x
  LThis -> failure x
  LThisDC -> failure x
  LStr string -> failure x
  LInt integer -> failure x
transEffExp :: EffExp -> Result
transEffExp x = case x of
  New type_ pureexps -> failure x
  NewLocal type_ pureexps -> failure x
  SyncMethCall pureexp lident pureexps -> failure x
  ThisSyncMethCall lident pureexps -> failure x
  AsyncMethCall pureexp lident pureexps -> failure x
  ThisAsyncMethCall lident pureexps -> failure x
  Get pureexp -> failure x
  ProGet pureexp -> failure x
  ProNew -> failure x
  ProEmpty pureexp -> failure x
  ProTry pureexp -> failure x
  Spawns pureexp type_ pureexps -> failure x
transAnnot :: Annot -> Result
transAnnot x = case x of
  Ann annot -> failure x
transAnnot_ :: Annot_ -> Result
transAnnot_ x = case x of
  AnnWithType type_ pureexp -> failure x
  AnnNoType pureexp -> failure x
transAnnotStm :: AnnotStm -> Result
transAnnotStm x = case x of
  AnnStm annots stm -> failure x
transAnnotDecl :: AnnotDecl -> Result
transAnnotDecl x = case x of
  AnnDec annots decl -> failure x
transAnnotMethSignat :: AnnotMethSignat -> Result
transAnnotMethSignat x = case x of
  AnnMethSig annots methsignat -> failure x


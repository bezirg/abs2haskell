-- | data structures carried during the compilation/translation
module Lang.ABS.Compiler.Base where

import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Data.Map as M (Map)
import Control.Monad.Trans.Reader (Reader, ReaderT)
import Control.Monad.Trans.State (State)

-- | Represents the module table, which is a list of all the modules info
data ModuleInfo = ModuleInfo {
      filePath :: FilePath,
      moduleName :: ABS.QualType,
      hierarchy :: M.Map ABS.TypeIdent [ABS.QualType], -- Interface -> Extends
      methods :: M.Map ABS.TypeIdent [ABS.Ident],      -- Interface -> Methods
      exceptions :: [ABS.TypeIdent]                    -- names of exceptions, needed for code generating smart-exception-constructors
    } deriving (Show)


type ModuleTable = [ModuleInfo]
-- TODO, change it to this
-- type ModuleTable = M.Map ABS.QualType ModuleInfo

type ScopeTable = M.Map ABS.Ident (ABS.Type)

type ExprM = Reader (ScopeTable -- current function scope
                    ,String -- interface name
                    )

type ExprLiftedM = Reader (ScopeTable -- current function scope
                    ,ScopeTable -- current class scope -- fscope `union` cscope == scope
                    ,ScopeTable -- current method-params
                    ,String -- interface name
                    ,Bool -- is init block? then it cannot use await and/or synchronous calls
                    )

type StmtM = ReaderT (ScopeTable -- current class scope
                     ,ScopeTable -- current method-params
                     ,String      -- interface name
                     ,String      -- class name
                     ,Bool     -- is init block? then it cannot use await and/or synchronous calls
                     )
    (State [ScopeTable])  -- all function block scopes
    

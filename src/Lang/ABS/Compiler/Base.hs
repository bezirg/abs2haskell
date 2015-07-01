-- | Data-structures carried during the compilation/translation, used by the a2h transcompiler
module Lang.ABS.Compiler.Base where

import qualified Lang.ABS.Compiler.BNFC.AbsABS as ABS
import qualified Data.Map as M (Map)
import Control.Monad.Trans.Reader (Reader, ReaderT)
import Control.Monad.Trans.State (State)

-- | Represents the info of 1 ABS module
-- 
-- It is created during a 1st-pass and used in the 2nd-pass
data ModuleInfo = ModuleInfo {
      filePath :: FilePath      -- ^ the filename of the ABS module, ending in .abs
    , moduleName :: ABS.QType   -- ^ the parsed name of the ABS module, e.g. module ParsedName;
    , hierarchy :: M.Map ABS.UIdent [ABS.QType] -- ^ A mapping of interface => to all the interfaces it _directly_ extends.
    , methods :: M.Map ABS.UIdent [ABS.LIdent]      -- ^ A mapping of interface => to all methods it _directly_ specifies
    , exceptions :: [ABS.UIdent]                    -- ^ names of exceptions, needed for code generating smart-exception-constructors
    , fimports :: [ABS.AnyIdent]                    -- ^ all the foreign-imports of that module (temp fix)
    , exports :: [ABS.AnyIdent]                     -- ^ all the ABS exports of that module
    } deriving (Show)


-- | The module table is a list of the 'ModuleInfo's of the ABS program.
type ModuleTable = [ModuleInfo]
-- TODO, change it to this
-- type ModuleTable = M.Map ABS.QType ModuleInfo

-- | The current scope is represented as a mapping of lower-case names => to their ABS-declared types
type ScopeTable = M.Map ABS.LIdent (ABS.Type)

-- * The 3 translation monads


-- ^ The simplest translation monad is the one that operates on functional-core ABS code
-- It holds 
--
-- 1. the current functional scope (introduced by let-local-variables and case)
-- 2. the interface name
--
-- It has no state
type ExprM = Reader ScopeTable -- the current functional scope (introduced by let-local-variables and case)

-- ^ The translation monad of _pure_ and _effectful_ expressions inside the ABS object-layer
-- It holds
--
-- 1. current functional scope (introduced by local variables)
-- 2. current class scope minus the functional scope, i.e. fscope `union` cscope == scope
-- 3. current method parameters
-- 4. the interface name that we are currently implementing with this class method
-- 5. is init block? then it cannot use await and/or synchronous calls
--
-- The translation always happens inside a method-code-block (also the main-block). It has no state
type ExprLiftedM = Reader (ScopeTable -- current functional scope (introduced by local variables)
                    ,ScopeTable -- current class scope, i.e. fscope `union` cscope == full_current_scope
                    ,ScopeTable -- current method parameters
                    ,String     -- the class name that we are currently implementing with this class method
                    ,Bool -- is init block? then it cannot use await and/or synchronous calls
                    ,[ABS.LIdent] -- a list of visible methods (exluding non-methods); needed by exprlifted thismethcalls
                    )

-- ^ The translation monad of ABS (monadic) statements, inside the ABS object-layer
-- 
-- The translation always happens inside a method-code-block (also the main-block). 
-- The reader monad is mostly about current scope:
--
-- 1. current class scope
-- 2. current method-params
-- 3. interface name
-- 4. class name
-- 5. is init block? then it cannot use await and/or synchronous calls
--
-- The state monad is a list (stack) of all the above block-scopes to return to after the current block finishes.
type StmtM = ReaderT (ScopeTable -- current class scope
                     ,ScopeTable -- current method-params
                     ,String      -- interface name
                     ,String      -- class name
                     ,Bool     -- is init block? then it cannot use await and/or synchronous calls
                     ,[ABS.LIdent] -- a list of visible methods (exluding non-methods); needed by exprlifted thismethcalls
                     )
    (State [ScopeTable])  -- all function block scopes
    

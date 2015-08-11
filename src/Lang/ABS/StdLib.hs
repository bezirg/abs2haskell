-- | The stdlib module is /implicitly/ imported by all ABS modules.
-- This module reexports 2 Haskell submodules:
--
-- 1. the ABS Prelude
-- 2. the DC-interface extension
module Lang.ABS.StdLib
    (module Lang.ABS.StdLib.Prelude
    ) where

import Lang.ABS.StdLib.Prelude

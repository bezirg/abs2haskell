{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Lang.ABS.Compiler.BNFC.ParABS where
import Lang.ABS.Compiler.BNFC.AbsABS
import Lang.ABS.Compiler.BNFC.LexABS
import Lang.ABS.Compiler.BNFC.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Ident) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Ident)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (String) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (String)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Integer) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Integer)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (TypeIdent) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (TypeIdent)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (AnyIdent) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (AnyIdent)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([AnyIdent]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([AnyIdent])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Program) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Program)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Module]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Module])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Module) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Module)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Export) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Export)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Export]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Export])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Import) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Import)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([Import]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([Import])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (ImportType) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (ImportType)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Type) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Type)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([Type]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([Type])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ([QualType]) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ([QualType])
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (QualType) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (QualType)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (QualTypeSegment) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (QualTypeSegment)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([QualTypeSegment]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([QualTypeSegment])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([Decl]) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([Decl])
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Decl) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Decl)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (ConstrIdent) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (ConstrIdent)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (ConstrType) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (ConstrType)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ([ConstrType]) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ([ConstrType])
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([TypeIdent]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ([TypeIdent])
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([ConstrIdent]) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([ConstrIdent])
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (FunBody) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (FunBody)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (MethSignat) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (MethSignat)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([MethSignat]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([MethSignat])
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (ClassBody) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (ClassBody)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ([ClassBody]) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ([ClassBody])
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Block) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Block)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (MaybeBlock) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (MaybeBlock)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([Param]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([Param])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Param) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Param)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: ([Stm]) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> ([Stm])
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Stm) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Stm)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Guard) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Guard)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Exp) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Exp)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([PureExp]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([PureExp])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (PureExp) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (PureExp)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (PureExp) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (PureExp)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (PureExp) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (PureExp)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (PureExp) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (PureExp)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (PureExp) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (PureExp)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (PureExp) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (PureExp)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (PureExp) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (PureExp)
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (PureExp) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (PureExp)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (CaseBranch) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (CaseBranch)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ([CaseBranch]) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> ([CaseBranch])
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: ([Pattern]) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> ([Pattern])
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (Pattern) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (Pattern)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (Literal) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (Literal)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (EffExp) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (EffExp)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x13\x03\x00\x00\x10\x03\x25\x03\x00\x00\x0f\x03\x00\x00\x42\x03\x43\x03\x00\x00\x00\x00\x0c\x03\x00\x00\x2b\x03\x00\x00\x3e\x03\xfe\x00\x09\x00\x00\x00\x00\x00\x3f\x03\x17\x03\x16\x03\x2c\x03\x05\x00\xde\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\x02\xf7\x02\x39\x00\xf7\x02\xf7\x02\x00\x00\x28\x03\x0e\x03\x24\x03\x0d\x03\x00\x00\xef\x02\xef\x02\x30\x01\x00\x00\x00\x00\x00\x00\xef\x02\x30\x01\xef\x02\x28\x00\x1e\x03\x49\x00\xea\x02\x1f\x03\x00\x00\xaf\x01\xfd\xff\x39\x00\xd2\x02\x00\x00\xd2\x02\xd2\x02\x39\x00\xd6\x00\xd2\x02\x00\x00\x39\x00\x24\x01\x00\x00\x00\x00\xd4\x02\xff\xff\x00\x00\x04\x03\x03\x00\x0a\x03\x08\x00\x7d\x02\x8a\x01\x31\x01\x00\x00\x00\x00\x00\x00\x00\x00\x68\x01\x7b\x01\x68\x01\x49\x01\x2b\x01\x0c\x01\x0b\x03\x97\x00\x00\x00\x92\x00\x00\x03\xff\x02\x1c\x01\x00\x00\x05\x03\x00\x00\x00\x00\x7b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x00\x71\x00\x00\x00\xfd\x02\xfb\xff\xee\x00\xbe\x02\xbe\x02\x00\x00\x00\x00\xeb\x02\xee\x00\x1a\x01\xee\x02\x39\x00\x39\x00\x69\x00\xee\x00\xd0\x00\xf1\x00\x45\x00\xb7\x02\xe6\x02\x04\x00\x00\x00\x1a\x00\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\x7b\x01\xb2\x02\xea\xff\x39\x00\x7b\x01\x00\x00\xee\x00\xb2\x02\x48\x01\xee\x00\x92\x00\xee\x00\xe1\x02\xa2\x00\xa3\x02\xd1\x02\x39\x00\x98\x02\xcf\x02\xc0\x02\xcd\x02\x97\x02\xc3\x02\xc6\x02\xbb\x02\x95\x00\x94\x02\x8e\x02\xc5\x02\xbf\x02\x39\x00\xe1\xff\x00\x00\x00\x00\x7c\x02\x00\x00\x00\x00\xac\x02\x78\x02\x00\x00\x78\x02\x39\x00\x00\x00\x39\x00\xa5\x02\xaa\x02\x59\x02\x00\x00\x5b\x02\xa2\x02\x00\x00\x00\x00\x81\x02\x3c\x00\x8f\x02\x8d\x02\x00\x00\x92\x00\x0d\x00\x8c\x02\x8b\x02\x89\x02\x88\x02\x00\x00\x83\x02\x08\x00\x7d\x02\x7d\x02\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x31\x01\x31\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x02\xcf\x00\x00\x00\x00\x00\xdd\x01\x19\x00\xb1\x00\x76\x02\x74\x02\xb1\x00\x3a\x02\x00\x00\xa7\x00\x6c\x02\x10\x00\x00\x00\x34\x02\x00\x00\x55\x00\xb1\x00\xb1\x00\x92\x00\x6b\x02\x69\x02\xb1\x00\x58\x02\xe7\xff\x55\x00\x00\x00\x57\x02\x46\x02\x1e\x02\x40\x02\x00\x00\x00\x00\x00\x00\x4b\x02\x2f\x02\xb1\x00\xb1\x00\xb1\x00\x00\x00\xb1\x00\xb1\x00\x3d\x02\x00\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x44\x02\xa1\x00\x00\x00\x33\x02\x37\x02\x00\x00\xfd\x01\x31\x02\x23\x02\x00\x00\x00\x00\xea\x01\x8a\x00\x0e\x01\x67\x00\xea\x01\x00\x00\x00\x00\x67\x00\xe8\x01\x00\x00\x39\x00\x00\x00\xb1\x00\x00\x00\x17\x02\x00\x00\x39\x00\x00\x00\x39\x00\x74\x00\x00\x00\x39\x00\x00\x00\x00\x00\x09\x02\x19\x02\x14\x02\x13\x02\x12\x02\x00\x00\xb1\x00\x00\x00\xdd\x01\xdd\x01\xef\x01\xb1\x00\xb1\x00\x08\x02\x00\x00\x00\x02\xf4\x01\xf2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xbb\x01\x55\x00\xee\x01\xe1\x01\x00\x00\xb8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\x01\xde\x01\xa4\x01\x00\x00\xd4\x01\x00\x00\x00\x00\xf4\xff\xd2\x01\x89\x00\x00\x00\x00\x00\x83\x00\x58\x00\x00\x00\x90\x01\x00\x00\xb7\x01\x00\x00\x00\x00\xdd\x01\x00\x00\x00\x00\xb1\x00\x87\x01\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x4b\x00\xb1\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xfd\x00\x00\x00\x00\x00\x00\x00\xb0\x01\x00\x00\x68\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x00\xad\x01\x17\x01\x00\x00\x00\x00\x46\x01\xaf\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\x02\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x01\x9d\x01\x13\x08\x9a\x01\x6e\x01\x07\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x03\x1a\x03\xa9\x02\x00\x00\x00\x00\x00\x00\x71\x01\x3d\x01\x65\x01\x35\x02\x00\x00\x00\x00\x99\x01\x00\x00\x00\x00\x00\x00\x00\x00\xab\x07\x09\x03\x77\x01\xdf\x00\xd3\x00\x15\x02\x00\x00\xe0\x01\x2c\x01\x0b\x08\x00\x00\x00\x00\x00\x00\x92\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x06\x72\x07\x55\x06\xd5\x04\x35\x06\x15\x06\x00\x00\x02\x08\x00\x00\xb5\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\x68\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x02\xf5\x05\x7e\x01\x67\x01\x00\x00\x00\x00\x00\x00\xd5\x05\x00\x00\x00\x00\x52\x07\x89\x01\x00\x00\xb5\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x07\x48\x07\x41\x07\x21\x07\xdc\x01\x0b\x07\x01\x07\xeb\x06\x3f\x02\xcb\x06\xb5\x06\xab\x06\x58\x01\x50\x01\x52\x01\x95\x06\x00\x00\xb5\x04\x3f\x01\x00\x00\x95\x04\x95\x02\x75\x04\x00\x00\xe0\x07\x00\x00\x00\x00\xa5\x07\xc7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x07\x00\x00\x3a\x01\x00\x00\x00\x00\x98\x07\x00\x00\x00\x00\x13\x01\x19\x01\x00\x00\x02\x01\x00\x00\x60\x00\x00\x00\xa9\x00\xfa\x07\x00\x00\xb3\x01\x00\x00\x00\x00\xd7\x01\x94\x00\xfa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\x00\x55\x04\x00\x00\x00\x00\x5c\x01\x00\x00\x95\x05\x00\x00\x00\x00\x35\x04\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x04\x02\x15\x04\xf5\x03\x55\x02\x00\x00\x00\x00\xd5\x03\x00\x00\x00\x00\xd3\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\x03\x95\x03\x75\x03\x00\x00\x55\x03\x35\x03\x00\x00\x00\x00\x00\x00\x15\x03\x00\x00\x00\x00\x00\x00\xe0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x3b\x02\x00\x00\xcf\x07\x34\x01\x7a\x00\x00\x00\xc6\x07\x00\x00\x6b\x00\x92\x07\x00\x00\x75\x05\x00\x00\x00\x00\x00\x00\xe8\x07\x00\x00\x7c\x07\xf5\x02\x00\x00\xe3\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x05\x00\x00\x1c\x00\x7a\x01\x00\x00\x35\x05\x15\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x02\x4e\x00\x3e\x00\x3b\x02\xbe\x07\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x00\x00\x00\x00\x00\xf5\x04\x00\x00\x00\x00\xd5\x02\x00\x00\xed\xff\x00\x00\x3b\x02\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf5\xff\x00\x00\xfe\xff\x00\x00\xf6\xff\xf4\xff\x00\x00\xdd\xff\x00\x00\xdc\xff\xde\xff\xfb\xff\x00\x00\xee\xff\xe9\xff\xdb\xff\x00\x00\xda\xff\x00\x00\xfa\xff\xf9\xff\xf8\xff\xf2\xff\xf0\xff\x00\x00\x00\x00\xb6\xff\xe7\xff\xe6\xff\xed\xff\xd9\xff\xb7\xff\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\xf9\xff\x00\x00\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\x00\x00\xf7\xff\xf1\xff\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xe5\xff\x00\x00\x00\x00\xb5\xff\x00\x00\xba\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\x00\x00\x7b\xff\x66\xff\x65\xff\x00\x00\x78\xff\xb0\xff\x00\x00\x9d\xff\x97\xff\x92\xff\x8f\xff\x8a\xff\x87\xff\x83\xff\x80\xff\x76\xff\x9c\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\xff\x00\x00\x00\x00\x00\x00\x68\xff\x67\xff\x00\x00\xb1\xff\xb8\xff\x00\x00\xfd\xff\xfc\xff\xeb\xff\xec\xff\xea\xff\x7b\xff\x78\xff\x82\xff\x68\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa5\xff\xa4\xff\x00\x00\x00\x00\x68\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\xff\x00\x00\x9f\xff\x68\xff\x00\x00\x81\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\xff\x9b\xff\x00\x00\x00\x00\x9b\xff\x00\x00\x9b\xff\x00\x00\x00\x00\x00\x00\xe0\xff\xb5\xff\x00\x00\xe2\xff\x00\x00\xcd\xff\xc4\xff\x00\x00\xc6\xff\x00\x00\xb6\xff\x00\x00\x00\x00\x00\x00\xb4\xff\xb5\xff\x00\x00\xb2\xff\xba\xff\x00\x00\xb9\xff\xba\xff\x00\x00\x00\x00\xd7\xff\x00\x00\xc9\xff\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\x00\x00\x00\x00\xd3\xff\xd8\xff\x00\x00\x9a\xff\x00\x00\x00\x00\xa9\xff\x00\x00\x79\xff\x00\x00\x98\xff\x00\x00\x00\x00\x5e\xff\x00\x00\x93\xff\x91\xff\x90\xff\x8b\xff\x8c\xff\x8d\xff\x8e\xff\x88\xff\x89\xff\x85\xff\x86\xff\x84\xff\x75\xff\xa3\xff\x00\x00\x00\x00\xa2\xff\xa1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\xff\x00\x00\xac\xff\x7a\xff\x00\x00\x00\x00\xae\xff\x00\x00\x7a\xff\x00\x00\x9b\xff\x9b\xff\x00\x00\x7a\xff\x00\x00\x9b\xff\x00\x00\x00\x00\x75\xff\x6e\xff\x6c\xff\x00\x00\x00\x00\x00\x00\x6d\xff\x6a\xff\x68\xff\x9e\xff\x7a\xff\x9b\xff\x9b\xff\x9b\xff\x77\xff\x9b\xff\x9b\xff\x00\x00\x7f\xff\xab\xff\x9b\xff\x7d\xff\xbe\xff\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\xe1\xff\xcb\xff\xc8\xff\x00\x00\xc3\xff\xc5\xff\x00\x00\x00\x00\x00\x00\xb6\xff\x00\x00\xba\xff\xb3\xff\xb6\xff\x00\x00\xba\xff\xb5\xff\xbd\xff\x00\x00\xd1\xff\x00\x00\xcc\xff\xc9\xff\xca\xff\xb5\xff\x00\x00\xd2\xff\xb5\xff\x99\xff\xa8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xff\x00\x00\x94\xff\x73\xff\x71\xff\xa7\xff\x00\x00\x00\x00\x00\x00\x64\xff\x00\x00\x00\x00\x00\x00\xad\xff\x5f\xff\x61\xff\xaa\xff\x63\xff\x00\x00\x95\xff\x00\x00\x00\x00\x70\xff\x72\xff\x74\xff\x60\xff\x62\xff\x5d\xff\x7e\xff\x7c\xff\x00\x00\x00\x00\xc1\xff\xc2\xff\x00\x00\xc7\xff\xd6\xff\x00\x00\x00\x00\x00\x00\xba\xff\xba\xff\x00\x00\xb6\xff\xcf\xff\x00\x00\xbc\xff\x00\x00\xd5\xff\xc0\xff\x71\xff\x6b\xff\xa6\xff\x00\x00\x96\xff\x6f\xff\x00\x00\xbb\xff\xba\xff\xd0\xff\x00\x00\x00\x00\xd4\xff\xce\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x06\x00\x0e\x00\x06\x00\x01\x00\x06\x00\x0b\x00\x2a\x00\x21\x00\x28\x00\x02\x00\x0c\x00\x1f\x00\x08\x00\x0f\x00\x0c\x00\x26\x00\x08\x00\x0e\x00\x06\x00\x19\x00\x1a\x00\x1b\x00\x07\x00\x1d\x00\x3c\x00\x12\x00\x3a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x07\x00\x27\x00\x3c\x00\x17\x00\x28\x00\x09\x00\x2c\x00\x0b\x00\x3f\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x06\x00\x34\x00\x3c\x00\x36\x00\x37\x00\x0b\x00\x39\x00\x3a\x00\x33\x00\x3a\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x3f\x00\x3c\x00\x3c\x00\x19\x00\x1a\x00\x1b\x00\x3f\x00\x1d\x00\x0a\x00\x42\x00\x3f\x00\x04\x00\x20\x00\x42\x00\x3c\x00\x31\x00\x32\x00\x27\x00\x34\x00\x35\x00\x19\x00\x0e\x00\x2c\x00\x3c\x00\x3c\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x06\x00\x34\x00\x1f\x00\x36\x00\x37\x00\x0b\x00\x39\x00\x3a\x00\x03\x00\x19\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x03\x00\x23\x00\x1f\x00\x19\x00\x1a\x00\x1b\x00\x19\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x06\x00\x3c\x00\x19\x00\x06\x00\x42\x00\x27\x00\x0c\x00\x16\x00\x0b\x00\x19\x00\x2c\x00\x1a\x00\x3a\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x3d\x00\x34\x00\x1f\x00\x36\x00\x37\x00\x42\x00\x39\x00\x3a\x00\x1c\x00\x1d\x00\x3a\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x1f\x00\x42\x00\x27\x00\x19\x00\x0b\x00\x35\x00\x00\x00\x2c\x00\x3a\x00\x19\x00\x19\x00\x30\x00\x3c\x00\x33\x00\x34\x00\x35\x00\x42\x00\x36\x00\x37\x00\x03\x00\x06\x00\x19\x00\x1d\x00\x19\x00\x1d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x11\x00\x27\x00\x19\x00\x19\x00\x0b\x00\x00\x00\x2c\x00\x16\x00\x3d\x00\x2f\x00\x30\x00\x1a\x00\x2d\x00\x42\x00\x3d\x00\x3d\x00\x36\x00\x37\x00\x03\x00\x42\x00\x42\x00\x00\x00\x1d\x00\x3a\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x03\x00\x42\x00\x27\x00\x42\x00\x0b\x00\x03\x00\x06\x00\x2c\x00\x3d\x00\x3d\x00\x19\x00\x30\x00\x03\x00\x42\x00\x42\x00\x0f\x00\x00\x00\x36\x00\x37\x00\x16\x00\x12\x00\x13\x00\x1d\x00\x1a\x00\x24\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x15\x00\x27\x00\x06\x00\x19\x00\x0b\x00\x00\x00\x2c\x00\x1e\x00\x1f\x00\x20\x00\x30\x00\x20\x00\x21\x00\x06\x00\x06\x00\x07\x00\x36\x00\x37\x00\x16\x00\x17\x00\x2b\x00\x3a\x00\x1d\x00\x3c\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x17\x00\x06\x00\x27\x00\x38\x00\x0b\x00\x3a\x00\x00\x00\x2c\x00\x01\x00\x0e\x00\x01\x00\x30\x00\x11\x00\x09\x00\x1f\x00\x24\x00\x0c\x00\x36\x00\x37\x00\x0c\x00\x29\x00\x0c\x00\x1d\x00\x06\x00\x24\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x1f\x00\x27\x00\x03\x00\x11\x00\x0b\x00\x03\x00\x2c\x00\x08\x00\x00\x00\x17\x00\x30\x00\x00\x00\x0d\x00\x00\x00\x03\x00\x04\x00\x36\x00\x37\x00\x10\x00\x11\x00\x12\x00\x13\x00\x1d\x00\x1d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x00\x00\x0b\x00\x27\x00\x0d\x00\x0b\x00\x03\x00\x0e\x00\x2c\x00\x00\x00\x11\x00\x14\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0e\x00\x36\x00\x37\x00\x11\x00\x12\x00\x13\x00\x1d\x00\x00\x00\x03\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x06\x00\x3f\x00\x27\x00\x03\x00\x42\x00\x0b\x00\x03\x00\x2c\x00\x11\x00\x12\x00\x13\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x36\x00\x37\x00\x06\x00\x11\x00\x12\x00\x13\x00\x1d\x00\x0b\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x03\x00\x31\x00\x32\x00\x27\x00\x34\x00\x35\x00\x00\x00\x09\x00\x2c\x00\x0b\x00\x1f\x00\x0e\x00\x30\x00\x00\x00\x11\x00\x12\x00\x13\x00\x03\x00\x36\x00\x37\x00\x03\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x30\x00\x23\x00\x33\x00\x34\x00\x35\x00\x0e\x00\x36\x00\x37\x00\x11\x00\x12\x00\x13\x00\x03\x00\x0a\x00\x08\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x0f\x00\x0e\x00\x11\x00\x0e\x00\x0f\x00\x3c\x00\x11\x00\x12\x00\x13\x00\x25\x00\x11\x00\x27\x00\x3a\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x35\x00\x36\x00\x07\x00\x03\x00\x07\x00\x00\x00\x01\x00\x02\x00\x03\x00\x3c\x00\x0e\x00\x07\x00\x03\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0a\x00\x0e\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x3c\x00\x07\x00\x19\x00\x3c\x00\x25\x00\x07\x00\x27\x00\x07\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x35\x00\x36\x00\x2e\x00\x2f\x00\x30\x00\x30\x00\x0e\x00\x07\x00\x21\x00\x35\x00\x0e\x00\x36\x00\x37\x00\x11\x00\x12\x00\x13\x00\x03\x00\x07\x00\x07\x00\x07\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x07\x00\x18\x00\x3a\x00\x0e\x00\x0f\x00\x0e\x00\x11\x00\x12\x00\x13\x00\x25\x00\x07\x00\x27\x00\x42\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x35\x00\x36\x00\x0a\x00\x3f\x00\x06\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0e\x00\x11\x00\x16\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x06\x00\x0e\x00\x11\x00\x12\x00\x13\x00\x04\x00\x11\x00\x12\x00\x13\x00\x13\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1e\x00\x25\x00\x3d\x00\x27\x00\x06\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x11\x00\x35\x00\x36\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x07\x00\x06\x00\x06\x00\x3f\x00\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\x3f\x00\x06\x00\x3f\x00\x27\x00\x07\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x06\x00\x35\x00\x36\x00\x0f\x00\x10\x00\x06\x00\x06\x00\x05\x00\x14\x00\x15\x00\x07\x00\x07\x00\x00\x00\x01\x00\x02\x00\x03\x00\x18\x00\x3f\x00\x42\x00\x27\x00\x0e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x00\x00\x35\x00\x36\x00\x03\x00\x04\x00\x05\x00\x00\x00\x0e\x00\x07\x00\x03\x00\x04\x00\x05\x00\x00\x00\x01\x00\x02\x00\x03\x00\x14\x00\x42\x00\x3f\x00\x27\x00\x11\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x0a\x00\x35\x00\x36\x00\x07\x00\x3f\x00\x3a\x00\x14\x00\x0a\x00\x0e\x00\x3b\x00\x06\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0a\x00\x42\x00\x0a\x00\x27\x00\x3a\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x00\x00\x35\x00\x36\x00\x03\x00\x04\x00\x05\x00\x0e\x00\x1b\x00\x3f\x00\x0c\x00\x3c\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0e\x00\x11\x00\x12\x00\x13\x00\x3f\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x0c\x00\x35\x00\x06\x00\x03\x00\x0e\x00\x0e\x00\x05\x00\x1b\x00\x06\x00\x0e\x00\x3f\x00\x42\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x11\x00\x12\x00\x13\x00\x03\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x3f\x00\x35\x00\x11\x00\x12\x00\x13\x00\x0f\x00\x11\x00\x0c\x00\x42\x00\x25\x00\x25\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x42\x00\x0e\x00\x25\x00\x25\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x0a\x00\x35\x00\x03\x00\x0e\x00\x22\x00\x42\x00\x0c\x00\x0e\x00\x42\x00\x3f\x00\x2e\x00\x44\x00\x00\x00\x01\x00\x02\x00\x03\x00\x11\x00\x12\x00\x13\x00\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x11\x00\x12\x00\x13\x00\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x26\x00\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x26\x00\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x03\x00\xff\xff\xff\xff\xff\xff\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\x22\x00\x23\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x2e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x03\x00\x35\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x11\x00\x12\x00\x13\x00\x2f\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x35\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\xff\xff\x35\x00\xff\xff\xff\xff\xff\xff\x03\x00\xff\xff\x2f\x00\x30\x00\xff\xff\xff\xff\x03\x00\xff\xff\x35\x00\x22\x00\x23\x00\x0e\x00\x2f\x00\x30\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x35\x00\x03\x00\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0e\x00\x22\x00\x23\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x22\x00\x23\x00\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x03\x00\xff\xff\xff\xff\x0e\x00\x22\x00\x23\x00\x11\x00\x12\x00\x13\x00\x03\x00\xff\xff\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x03\x00\xff\xff\x1e\x00\x0e\x00\x20\x00\x21\x00\x11\x00\x12\x00\x13\x00\x03\x00\x1e\x00\x0e\x00\x20\x00\x21\x00\x11\x00\x12\x00\x13\x00\x03\x00\xff\xff\x1e\x00\x0e\x00\x20\x00\x21\x00\x11\x00\x12\x00\x13\x00\xff\xff\x1e\x00\x0e\x00\x20\x00\x21\x00\x11\x00\x12\x00\x13\x00\x1c\x00\x03\x00\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\xff\xff\xff\xff\x03\x00\xff\xff\xff\xff\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x03\x00\xff\xff\x0e\x00\x17\x00\x18\x00\x11\x00\x12\x00\x13\x00\x03\x00\xff\xff\xff\xff\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x0e\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x59\x00\x6c\x01\x3e\x00\x96\x00\x9b\x00\x5a\x00\x73\x01\x44\x01\x25\x01\x93\x00\x9c\x00\x79\x01\x2b\x00\x43\x00\x97\x00\xd4\x00\x18\x00\xe3\x00\x0e\x01\x3b\x00\x5b\x00\x5c\x00\xf6\x00\x5d\x00\x99\x00\x94\x00\x26\x01\xff\x00\x48\x00\x49\x00\x00\x01\xff\x00\xe2\x00\x5e\x00\x99\x00\x0f\x01\x3f\x00\x8d\x00\x5f\x00\x8e\x00\x03\x00\x60\x00\x61\x00\x62\x00\x63\x00\x59\x00\x64\x00\x99\x00\x65\x00\x66\x00\x5a\x00\x67\x00\x68\x00\x98\x00\x40\x00\xf3\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\xe4\xff\x99\x00\x99\x00\x3b\x00\x5b\x00\x5c\x00\x03\x00\x5d\x00\x13\x01\x0c\x00\x03\x00\xe5\x00\x76\x01\x0c\x00\x99\x00\x01\x01\x54\x01\x5e\x00\x03\x01\x04\x01\x3b\x00\xe6\x00\x5f\x00\x99\x00\x99\x00\x60\x00\x61\x00\x62\x00\x63\x00\x59\x00\x64\x00\x67\x01\x65\x00\x66\x00\x5a\x00\x67\x00\x68\x00\xab\x00\x3b\x00\x69\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\xa8\x00\x45\x00\x68\x01\x3b\x00\x5b\x00\x5c\x00\x3b\x00\x5d\x00\xff\x00\x48\x00\x49\x00\x00\x01\x9b\x00\x99\x00\x1f\x01\x59\x00\x0c\x00\x5e\x00\x9c\x00\xa9\x00\x5a\x00\x3b\x00\x5f\x00\x2e\x01\x46\x00\x60\x00\x61\x00\x62\x00\x63\x00\x7d\x01\x64\x00\x64\x01\x65\x00\x66\x00\x0c\x00\x67\x00\x68\x00\x5f\x01\x5d\x00\x27\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\x27\x01\x0c\x00\x7b\x00\x3b\x00\x5a\x00\xea\x00\x31\x01\x5f\x00\x27\x00\x3b\x00\x3b\x00\x61\x00\x99\x00\x74\x01\x53\x01\x04\x01\x0c\x00\x73\x00\x66\x00\xa8\x00\xf8\x00\x3b\x00\x5d\x00\x3b\x00\x16\x01\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\xf9\x00\x7b\x00\x3b\x00\x3b\x00\x5a\x00\xf4\x00\x5f\x00\xa9\x00\x79\x01\x60\x00\x61\x00\x1e\x01\x7e\x00\x0c\x00\x6a\x01\x2e\x01\x7c\x00\x66\x00\xab\x00\x0c\x00\x0c\x00\xf9\x00\x5d\x00\x27\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\xa8\x00\x0c\x00\x7b\x00\x0c\x00\x5a\x00\x07\x00\xa5\x00\x5f\x00\x35\x01\xc7\x00\xc0\x00\x61\x00\xab\x00\x0c\x00\x0c\x00\xa6\x00\x08\x01\x73\x00\x66\x00\xa9\x00\x09\x00\x0f\x00\x5d\x00\xaa\x00\x73\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\x1e\x00\x7b\x00\x9e\x00\xac\x00\x5a\x00\x15\x01\x5f\x00\x22\x00\x23\x00\x24\x00\x61\x00\x1f\x00\x20\x00\x9e\x00\x03\x00\x04\x00\x86\x00\x66\x00\xe7\x00\xa0\x00\x25\x00\xe8\x00\x5d\x00\x99\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x81\x00\xa0\x00\x2b\x01\x7b\x00\x26\x00\x5a\x00\x27\x00\x22\x01\x5f\x00\x76\x00\x2c\x01\x76\x00\x61\x00\x2d\x01\x10\x00\x21\x01\x1c\x00\x11\x00\x73\x00\x66\x00\xee\x00\x1d\x00\x77\x00\x5d\x00\x9e\x00\x35\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\x23\x01\x7b\x00\x8a\x00\x9f\x00\x5a\x00\x07\x00\x5f\x00\x8b\x00\xb4\x00\xa0\x00\x61\x00\x13\x00\x8c\x00\xce\x00\x14\x00\x6d\x00\x73\x00\x66\x00\x28\x01\xa3\x00\x09\x00\x0a\x00\x5d\x00\xa1\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\xd2\x00\x18\x00\x7b\x00\x19\x00\x5a\x00\x07\x00\xcd\x00\x5f\x00\xd4\x00\xce\x00\x1a\x00\x61\x00\xff\x00\x48\x00\x49\x00\x00\x01\xd1\x00\x73\x00\x66\x00\x39\x00\x09\x00\x0a\x00\x5d\x00\xef\x00\x07\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x59\x00\x03\x00\x7b\x00\x36\x00\x0c\x00\x5a\x00\x07\x00\x5f\x00\x6c\x00\x09\x00\x0a\x00\x61\x00\xff\x00\x48\x00\x49\x00\x00\x01\xf0\x00\x86\x00\x66\x00\x59\x00\x6e\x00\x09\x00\x0a\x00\x5d\x00\x5a\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x07\x00\x01\x01\x02\x01\x7b\x00\x03\x01\x04\x01\x9c\x00\x8d\x00\x5f\x00\x8e\x00\xad\x00\xaf\x00\x61\x00\x43\x00\x39\x00\x09\x00\x0a\x00\x37\x00\x73\x00\x66\x00\x3b\x00\x3c\x00\x47\x00\x48\x00\x49\x00\x07\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x61\x00\xea\x00\x52\x01\x53\x01\x04\x01\x4a\x00\x73\x00\x66\x00\x4b\x00\x09\x00\x0a\x00\x07\x00\x0e\x00\x05\x00\x6a\x00\x03\x00\x6b\x00\x6c\x00\x0c\x00\x41\x00\x7c\x01\x42\x00\xa6\x00\x1a\x01\x99\x00\x39\x00\x09\x00\x0a\x00\x71\x01\x76\x01\x4d\x00\x27\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x47\x00\x48\x00\x49\x00\x07\x00\x56\x00\x57\x00\x6b\x01\x07\x00\x6d\x01\x6f\x00\x48\x00\x49\x00\x07\x00\x99\x00\x4a\x00\x6f\x01\x07\x00\x4b\x00\x09\x00\x0a\x00\x17\x01\xa3\x00\x09\x00\x0a\x00\x70\x01\x6e\x01\x70\x00\x09\x00\x0a\x00\xa2\x00\xa3\x00\x09\x00\x0a\x00\x99\x00\x71\x01\x06\x01\x99\x00\x42\x01\x4c\x01\x4d\x00\x4d\x01\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x47\x00\x48\x00\x49\x00\x07\x00\x56\x00\x57\x00\xdc\x00\x54\x00\x55\x00\x61\x00\x4e\x01\x4f\x01\x52\x01\x56\x00\x4a\x00\x07\x01\x66\x00\x4b\x00\x09\x00\x0a\x00\x07\x00\x57\x01\x58\x01\x59\x01\x03\x00\x6b\x00\x6c\x00\x0c\x00\x5a\x01\x5b\x01\x66\x01\xa6\x00\xa7\x00\x62\x01\x39\x00\x09\x00\x0a\x00\x4a\x01\x30\x01\x4d\x00\x0c\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x47\x00\x48\x00\x49\x00\x07\x00\x56\x00\x57\x00\x31\x01\x03\x00\x33\x01\x07\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x4a\x00\x34\x01\x3e\x01\x4b\x00\x09\x00\x0a\x00\xb6\x00\x36\x01\x38\x01\x39\x00\x09\x00\x0a\x00\xe5\x00\x70\x00\x09\x00\x0a\x00\x3f\x01\x41\x01\x6f\x00\x48\x00\x49\x00\x07\x00\xb7\x00\x4c\x00\x40\x01\x4d\x00\x42\x01\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x45\x01\x56\x00\x57\x00\xd8\x00\x53\x00\x54\x00\x55\x00\x47\x01\xf8\x00\xf7\x00\x03\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x03\x00\xfc\x00\x03\x00\x47\x01\xfd\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x0a\x01\x56\x00\x57\x00\x8f\x00\x90\x00\x0b\x01\x0c\x01\x95\x00\x91\x00\x92\x00\x0d\x01\x11\x01\x6f\x00\x48\x00\x49\x00\x07\x00\x14\x01\x03\x00\x0c\x00\x0f\x01\x12\x01\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x13\x00\x56\x00\x57\x00\x14\x00\x15\x00\x2f\x00\x13\x00\x15\x01\x19\x01\x14\x00\x15\x00\x16\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x1a\x01\x0c\x00\x03\x00\xca\x00\x21\x01\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\xb3\x00\x56\x00\x57\x00\xb4\x00\x03\x00\xb6\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\x6f\x00\x48\x00\x49\x00\x07\x00\xc0\x00\x0c\x00\xc3\x00\x79\x00\xc4\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x13\x00\x56\x00\x57\x00\x27\x00\x15\x00\x28\x00\xc8\x00\x7a\x01\x03\x00\xe4\x00\x99\x00\xed\x00\x6f\x00\x48\x00\x49\x00\x07\x00\xef\x00\x29\x00\x09\x00\x0a\x00\x03\x00\x5d\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\xf4\x00\x56\x00\x75\x00\x07\x00\x78\x00\x79\x00\x95\x00\x5c\x01\x7f\x00\x9a\x00\x03\x00\x0c\x00\x6f\x00\x48\x00\x49\x00\x07\x00\xae\x00\xa3\x00\x09\x00\x0a\x00\x07\x00\x5d\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x03\x00\x56\x00\x30\x00\x09\x00\x0a\x00\x43\x00\x47\x00\x34\x00\x0c\x00\x33\x00\x35\x00\xdd\xff\x6f\x00\x48\x00\x49\x00\x07\x00\x0c\x00\x2c\x00\x2d\x00\x2e\x00\x36\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x2f\x00\x56\x00\x07\x00\x1e\x00\x13\x00\x0c\x00\x0d\x00\x0e\x00\x0c\x00\x03\x00\x07\x00\xff\xff\x6f\x00\x48\x00\x49\x00\x07\x00\x31\x00\x09\x00\x0a\x00\x00\x00\x38\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x39\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x01\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\x00\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x07\x01\x00\x00\x00\x00\x84\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x00\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\x00\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\x00\xc9\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x83\x00\x00\x00\x00\x00\x84\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x62\x01\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x70\x00\x09\x00\x0a\x00\xd0\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\xd6\x00\x52\x00\x53\x00\x54\x00\x55\x00\x07\x00\x00\x00\x00\x00\x00\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x39\x00\x09\x00\x0a\x00\xd7\x00\x52\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x5b\x01\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x00\x53\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\xdb\x00\x53\x00\x54\x00\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x00\x00\x00\x00\xdd\x00\x54\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x07\x00\x56\x00\x00\x00\x00\x00\x70\x00\x09\x00\x0a\x00\x6f\x00\x48\x00\x49\x00\x07\x00\xeb\x00\x00\x00\x00\x00\x39\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x00\x00\x70\x00\x09\x00\x0a\x00\xde\x00\x55\x00\x6f\x00\x48\x00\x49\x00\x07\x00\x56\x00\xdf\x00\x55\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x70\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\xe0\x00\x55\x00\x39\x00\x09\x00\x0a\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x71\x00\x55\x00\x00\x00\x00\x00\x07\x00\x00\x00\x56\x00\x5f\x01\xb1\x00\xaf\x00\x87\x00\x55\x00\x39\x00\x09\x00\x0a\x00\xaf\x00\x56\x00\x07\x00\x39\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x63\x01\xb1\x00\x39\x00\x09\x00\x0a\x00\xaf\x00\x26\x01\xb1\x00\x39\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\x00\xb1\x00\x07\x00\x00\x00\x00\x00\xb6\x00\xb0\x00\xb1\x00\x39\x00\x09\x00\x0a\x00\x07\x00\x00\x00\xb6\x00\x00\x00\x00\x00\x39\x00\x09\x00\x0a\x00\x07\x00\x00\x00\xb7\x00\xb6\x00\x1f\x00\x77\x01\x39\x00\x09\x00\x0a\x00\x07\x00\xb7\x00\xb6\x00\x1f\x00\x66\x01\x39\x00\x09\x00\x0a\x00\x07\x00\x00\x00\xb7\x00\xc4\x00\x1f\x00\x29\x01\x39\x00\x09\x00\x0a\x00\x00\x00\xb7\x00\x1b\x01\x1f\x00\xb8\x00\x39\x00\x09\x00\x0a\x00\xc5\x00\x07\x00\x00\x00\x1c\x01\x60\x01\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x1b\x01\x00\x00\x00\x00\x39\x00\x09\x00\x0a\x00\x07\x00\x00\x00\x7c\x00\x1c\x01\x1d\x01\x39\x00\x09\x00\x0a\x00\x07\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x39\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x39\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 162) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162)
	]

happy_n_terms = 69 :: Int
happy_n_nonterms = 55 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn4
		 (Ident happy_var_1
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn6
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_TypeIdent happy_var_1)) -> 
	happyIn7
		 (TypeIdent (happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (AnyIden happy_var_1
	)}

happyReduce_6 = happySpecReduce_1  4# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (AnyTyIden happy_var_1
	)}

happyReduce_7 = happySpecReduce_1  5# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((:[]) happy_var_1
	)}

happyReduce_8 = happySpecReduce_3  5# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_1  6# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (Prog (reverse happy_var_1)
	)}

happyReduce_10 = happySpecReduce_0  7# happyReduction_10
happyReduction_10  =  happyIn11
		 ([]
	)

happyReduce_11 = happySpecReduce_2  7# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_12 = happyReduce 7# 8# happyReduction_12
happyReduction_12 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	case happyOut24 happy_x_6 of { happy_var_6 -> 
	case happyOut37 happy_x_7 of { happy_var_7 -> 
	happyIn12
		 (Modul happy_var_2 (reverse happy_var_4) (reverse happy_var_5) (reverse happy_var_6) happy_var_7
	) `HappyStk` happyRest}}}}}

happyReduce_13 = happySpecReduce_2  9# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (AnyExport happy_var_2
	)}

happyReduce_14 = happyReduce 4# 9# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (AnyFromExport happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_15 = happySpecReduce_2  9# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  happyIn13
		 (StarExport
	)

happyReduce_16 = happyReduce 4# 9# happyReduction_16
happyReduction_16 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn13
		 (StarFromExport happy_var_4
	) `HappyStk` happyRest}

happyReduce_17 = happySpecReduce_0  10# happyReduction_17
happyReduction_17  =  happyIn14
		 ([]
	)

happyReduce_18 = happySpecReduce_3  10# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_19 = happyReduce 4# 11# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut8 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (AnyImport happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_20 = happyReduce 4# 11# happyReduction_20
happyReduction_20 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (AnyFromImport happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_21 = happyReduce 4# 11# happyReduction_21
happyReduction_21 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 (StarFromImport happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_22 = happySpecReduce_0  12# happyReduction_22
happyReduction_22  =  happyIn16
		 ([]
	)

happyReduce_23 = happySpecReduce_3  12# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_24 = happySpecReduce_1  13# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn17
		 (ForeignImport
	)

happyReduce_25 = happySpecReduce_1  13# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn17
		 (NormalImport
	)

happyReduce_26 = happySpecReduce_1  14# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn18
		 (TUnderscore
	)

happyReduce_27 = happySpecReduce_1  14# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (TSimple happy_var_1
	)}

happyReduce_28 = happyReduce 4# 14# happyReduction_28
happyReduction_28 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (TGen happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_29 = happySpecReduce_1  15# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((:[]) happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  15# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_1  16# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((:[]) happy_var_1
	)}

happyReduce_32 = happySpecReduce_3  16# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_1  17# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (QType happy_var_1
	)}

happyReduce_34 = happySpecReduce_1  18# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (QTypeSegment happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  19# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ((:[]) happy_var_1
	)}

happyReduce_36 = happySpecReduce_3  19# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_37 = happySpecReduce_0  20# happyReduction_37
happyReduction_37  =  happyIn24
		 ([]
	)

happyReduce_38 = happySpecReduce_2  20# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_39 = happyReduce 5# 21# happyReduction_39
happyReduction_39 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (TypeDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_40 = happyReduce 5# 21# happyReduction_40
happyReduction_40 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (DataDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_41 = happyReduce 8# 21# happyReduction_41
happyReduction_41 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	case happyOut30 happy_x_7 of { happy_var_7 -> 
	happyIn25
		 (DataParDecl happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_42 = happyReduce 9# 21# happyReduction_42
happyReduction_42 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_8 of { happy_var_8 -> 
	happyIn25
		 (FunDecl happy_var_2 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_43 = happyReduce 12# 21# happyReduction_43
happyReduction_43 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	case happyOut38 happy_x_8 of { happy_var_8 -> 
	case happyOut31 happy_x_11 of { happy_var_11 -> 
	happyIn25
		 (FunParDecl happy_var_2 happy_var_3 happy_var_5 happy_var_8 happy_var_11
	) `HappyStk` happyRest}}}}}

happyReduce_44 = happyReduce 5# 21# happyReduction_44
happyReduction_44 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (InterfDecl happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_45 = happyReduce 7# 21# happyReduction_45
happyReduction_45 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	case happyOut33 happy_x_6 of { happy_var_6 -> 
	happyIn25
		 (ExtendsDecl happy_var_2 happy_var_4 (reverse happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_46 = happyReduce 7# 21# happyReduction_46
happyReduction_46 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	case happyOut37 happy_x_5 of { happy_var_5 -> 
	case happyOut35 happy_x_6 of { happy_var_6 -> 
	happyIn25
		 (ClassDecl happy_var_2 (reverse happy_var_4) happy_var_5 (reverse happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_47 = happyReduce 10# 21# happyReduction_47
happyReduction_47 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	case happyOut35 happy_x_7 of { happy_var_7 -> 
	case happyOut37 happy_x_8 of { happy_var_8 -> 
	case happyOut35 happy_x_9 of { happy_var_9 -> 
	happyIn25
		 (ClassParamDecl happy_var_2 happy_var_4 (reverse happy_var_7) happy_var_8 (reverse happy_var_9)
	) `HappyStk` happyRest}}}}}

happyReduce_48 = happyReduce 9# 21# happyReduction_48
happyReduction_48 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	case happyOut35 happy_x_6 of { happy_var_6 -> 
	case happyOut37 happy_x_7 of { happy_var_7 -> 
	case happyOut35 happy_x_8 of { happy_var_8 -> 
	happyIn25
		 (ClassImplements happy_var_2 happy_var_4 (reverse happy_var_6) happy_var_7 (reverse happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_49 = happyReduce 12# 21# happyReduction_49
happyReduction_49 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	case happyOut20 happy_x_7 of { happy_var_7 -> 
	case happyOut35 happy_x_9 of { happy_var_9 -> 
	case happyOut37 happy_x_10 of { happy_var_10 -> 
	case happyOut35 happy_x_11 of { happy_var_11 -> 
	happyIn25
		 (ClassParamImplements happy_var_2 happy_var_4 happy_var_7 (reverse happy_var_9) happy_var_10 (reverse happy_var_11)
	) `HappyStk` happyRest}}}}}}

happyReduce_50 = happySpecReduce_1  22# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (SinglConstrIdent happy_var_1
	)}

happyReduce_51 = happyReduce 4# 22# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (ParamConstrIdent happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_52 = happySpecReduce_1  23# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (EmptyConstrType happy_var_1
	)}

happyReduce_53 = happySpecReduce_2  23# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (RecordConstrType happy_var_1 happy_var_2
	)}}

happyReduce_54 = happySpecReduce_0  24# happyReduction_54
happyReduction_54  =  happyIn28
		 ([]
	)

happyReduce_55 = happySpecReduce_1  24# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((:[]) happy_var_1
	)}

happyReduce_56 = happySpecReduce_3  24# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_57 = happySpecReduce_1  25# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((:[]) happy_var_1
	)}

happyReduce_58 = happySpecReduce_3  25# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_59 = happySpecReduce_1  26# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((:[]) happy_var_1
	)}

happyReduce_60 = happySpecReduce_3  26# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_61 = happySpecReduce_1  27# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn31
		 (BuiltinFunBody
	)

happyReduce_62 = happySpecReduce_1  27# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (NormalFunBody happy_var_1
	)}

happyReduce_63 = happyReduce 5# 28# happyReduction_63
happyReduction_63 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	happyIn32
		 (MethSig happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_64 = happySpecReduce_0  29# happyReduction_64
happyReduction_64  =  happyIn33
		 ([]
	)

happyReduce_65 = happySpecReduce_3  29# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_66 = happySpecReduce_3  30# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (FieldClassBody happy_var_1 happy_var_2
	)}}

happyReduce_67 = happyReduce 5# 30# happyReduction_67
happyReduction_67 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut45 happy_x_4 of { happy_var_4 -> 
	happyIn34
		 (FieldAssignClassBody happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_68 = happyReduce 6# 30# happyReduction_68
happyReduction_68 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	case happyOut36 happy_x_6 of { happy_var_6 -> 
	happyIn34
		 (MethClassBody happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_69 = happySpecReduce_0  31# happyReduction_69
happyReduction_69  =  happyIn35
		 ([]
	)

happyReduce_70 = happySpecReduce_2  31# happyReduction_70
happyReduction_70 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_71 = happySpecReduce_3  32# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (Bloc (reverse happy_var_2)
	)}

happyReduce_72 = happySpecReduce_1  33# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (JustBlock happy_var_1
	)}

happyReduce_73 = happySpecReduce_0  33# happyReduction_73
happyReduction_73  =  happyIn37
		 (NoBlock
	)

happyReduce_74 = happySpecReduce_0  34# happyReduction_74
happyReduction_74  =  happyIn38
		 ([]
	)

happyReduce_75 = happySpecReduce_1  34# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  34# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_2  35# happyReduction_77
happyReduction_77 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn39
		 (Par happy_var_1 happy_var_2
	)}}

happyReduce_78 = happySpecReduce_0  36# happyReduction_78
happyReduction_78  =  happyIn40
		 ([]
	)

happyReduce_79 = happySpecReduce_2  36# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_80 = happySpecReduce_2  37# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (SExp happy_var_1
	)}

happyReduce_81 = happySpecReduce_3  37# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (SBlock (reverse happy_var_2)
	)}

happyReduce_82 = happyReduce 5# 37# happyReduction_82
happyReduction_82 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	happyIn41
		 (SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_83 = happySpecReduce_3  37# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (SReturn happy_var_2
	)}

happyReduce_84 = happyReduce 4# 37# happyReduction_84
happyReduction_84 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (SAss happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_85 = happyReduce 6# 37# happyReduction_85
happyReduction_85 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	happyIn41
		 (SFieldAss happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_86 = happySpecReduce_3  37# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (SDec happy_var_1 happy_var_2
	)}}

happyReduce_87 = happyReduce 5# 37# happyReduction_87
happyReduction_87 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn41
		 (SDecAss happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_88 = happyReduce 5# 37# happyReduction_88
happyReduction_88 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	happyIn41
		 (SIf happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_89 = happyReduce 7# 37# happyReduction_89
happyReduction_89 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	case happyOut41 happy_x_7 of { happy_var_7 -> 
	happyIn41
		 (SIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_90 = happySpecReduce_2  37# happyReduction_90
happyReduction_90 happy_x_2
	happy_x_1
	 =  happyIn41
		 (SSuspend
	)

happyReduce_91 = happySpecReduce_2  37# happyReduction_91
happyReduction_91 happy_x_2
	happy_x_1
	 =  happyIn41
		 (SSkip
	)

happyReduce_92 = happySpecReduce_3  37# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (SAssert happy_var_2
	)}

happyReduce_93 = happySpecReduce_3  37# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (SAwait happy_var_2
	)}

happyReduce_94 = happySpecReduce_2  38# happyReduction_94
happyReduction_94 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (VarGuard happy_var_1
	)}

happyReduce_95 = happyReduce 4# 38# happyReduction_95
happyReduction_95 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (FieldGuard happy_var_3
	) `HappyStk` happyRest}

happyReduce_96 = happySpecReduce_1  38# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (ExpGuard happy_var_1
	)}

happyReduce_97 = happySpecReduce_3  38# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (AndGuard happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_1  39# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (ExpP happy_var_1
	)}

happyReduce_99 = happySpecReduce_1  39# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (ExpE happy_var_1
	)}

happyReduce_100 = happySpecReduce_0  40# happyReduction_100
happyReduction_100  =  happyIn44
		 ([]
	)

happyReduce_101 = happySpecReduce_1  40# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((:[]) happy_var_1
	)}

happyReduce_102 = happySpecReduce_3  40# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_103 = happySpecReduce_3  41# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (EOr happy_var_1 happy_var_3
	)}}

happyReduce_104 = happySpecReduce_1  41# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_105 = happyReduce 8# 41# happyReduction_105
happyReduction_105 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOut45 happy_x_6 of { happy_var_6 -> 
	case happyOut45 happy_x_8 of { happy_var_8 -> 
	happyIn45
		 (Let happy_var_3 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_106 = happyReduce 6# 41# happyReduction_106
happyReduction_106 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut45 happy_x_4 of { happy_var_4 -> 
	case happyOut45 happy_x_6 of { happy_var_6 -> 
	happyIn45
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_107 = happyReduce 5# 41# happyReduction_107
happyReduction_107 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut54 happy_x_4 of { happy_var_4 -> 
	happyIn45
		 (Case happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_108 = happySpecReduce_3  42# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (EAnd happy_var_1 happy_var_3
	)}}

happyReduce_109 = happySpecReduce_1  42# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_110 = happySpecReduce_3  43# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (EEq happy_var_1 happy_var_3
	)}}

happyReduce_111 = happySpecReduce_3  43# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (ENeq happy_var_1 happy_var_3
	)}}

happyReduce_112 = happySpecReduce_1  43# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_113 = happySpecReduce_3  44# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (ELt happy_var_1 happy_var_3
	)}}

happyReduce_114 = happySpecReduce_3  44# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (ELe happy_var_1 happy_var_3
	)}}

happyReduce_115 = happySpecReduce_3  44# happyReduction_115
happyReduction_115 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (EGt happy_var_1 happy_var_3
	)}}

happyReduce_116 = happySpecReduce_3  44# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (EGe happy_var_1 happy_var_3
	)}}

happyReduce_117 = happySpecReduce_1  44# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_118 = happySpecReduce_3  45# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (EAdd happy_var_1 happy_var_3
	)}}

happyReduce_119 = happySpecReduce_3  45# happyReduction_119
happyReduction_119 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (ESub happy_var_1 happy_var_3
	)}}

happyReduce_120 = happySpecReduce_1  45# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_121 = happySpecReduce_3  46# happyReduction_121
happyReduction_121 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (EMul happy_var_1 happy_var_3
	)}}

happyReduce_122 = happySpecReduce_3  46# happyReduction_122
happyReduction_122 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (EDiv happy_var_1 happy_var_3
	)}}

happyReduce_123 = happySpecReduce_3  46# happyReduction_123
happyReduction_123 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (EMod happy_var_1 happy_var_3
	)}}

happyReduce_124 = happySpecReduce_1  46# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_125 = happySpecReduce_2  47# happyReduction_125
happyReduction_125 happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_2 of { happy_var_2 -> 
	happyIn51
		 (ELogNeg happy_var_2
	)}

happyReduce_126 = happySpecReduce_2  47# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_2 of { happy_var_2 -> 
	happyIn51
		 (EIntNeg happy_var_2
	)}

happyReduce_127 = happySpecReduce_1  47# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_128 = happyReduce 4# 48# happyReduction_128
happyReduction_128 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (EFunCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_129 = happyReduce 6# 48# happyReduction_129
happyReduction_129 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn52
		 (EQualFunCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_130 = happyReduce 4# 48# happyReduction_130
happyReduction_130 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (ENaryFunCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_131 = happyReduce 6# 48# happyReduction_131
happyReduction_131 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn52
		 (ENaryQualFunCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_132 = happySpecReduce_1  48# happyReduction_132
happyReduction_132 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (EVar happy_var_1
	)}

happyReduce_133 = happySpecReduce_3  48# happyReduction_133
happyReduction_133 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (EThis happy_var_3
	)}

happyReduce_134 = happySpecReduce_3  48# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (EQualVar happy_var_1 happy_var_3
	)}}

happyReduce_135 = happySpecReduce_1  48# happyReduction_135
happyReduction_135 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (ESinglConstr happy_var_1
	)}

happyReduce_136 = happyReduce 4# 48# happyReduction_136
happyReduction_136 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (EParamConstr happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_137 = happySpecReduce_1  48# happyReduction_137
happyReduction_137 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (ELit happy_var_1
	)}

happyReduce_138 = happySpecReduce_3  48# happyReduction_138
happyReduction_138 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (happy_var_2
	)}

happyReduce_139 = happySpecReduce_3  49# happyReduction_139
happyReduction_139 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (CaseBranc happy_var_1 happy_var_3
	)}}

happyReduce_140 = happySpecReduce_2  50# happyReduction_140
happyReduction_140 happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 ((:[]) happy_var_1
	)}

happyReduce_141 = happySpecReduce_3  50# happyReduction_141
happyReduction_141 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_142 = happySpecReduce_0  51# happyReduction_142
happyReduction_142  =  happyIn55
		 ([]
	)

happyReduce_143 = happySpecReduce_1  51# happyReduction_143
happyReduction_143 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 ((:[]) happy_var_1
	)}

happyReduce_144 = happySpecReduce_3  51# happyReduction_144
happyReduction_144 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_145 = happySpecReduce_1  52# happyReduction_145
happyReduction_145 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (PIdent happy_var_1
	)}

happyReduce_146 = happySpecReduce_1  52# happyReduction_146
happyReduction_146 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (PLit happy_var_1
	)}

happyReduce_147 = happySpecReduce_1  52# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (PSinglConstr happy_var_1
	)}

happyReduce_148 = happyReduce 4# 52# happyReduction_148
happyReduction_148 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (PParamConstr happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_149 = happySpecReduce_1  52# happyReduction_149
happyReduction_149 happy_x_1
	 =  happyIn56
		 (PUnderscore
	)

happyReduce_150 = happySpecReduce_1  53# happyReduction_150
happyReduction_150 happy_x_1
	 =  happyIn57
		 (LNull
	)

happyReduce_151 = happySpecReduce_1  53# happyReduction_151
happyReduction_151 happy_x_1
	 =  happyIn57
		 (LThis
	)

happyReduce_152 = happySpecReduce_1  53# happyReduction_152
happyReduction_152 happy_x_1
	 =  happyIn57
		 (LThisDC
	)

happyReduce_153 = happySpecReduce_1  53# happyReduction_153
happyReduction_153 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (LStr happy_var_1
	)}

happyReduce_154 = happySpecReduce_1  53# happyReduction_154
happyReduction_154 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (LInt happy_var_1
	)}

happyReduce_155 = happyReduce 5# 54# happyReduction_155
happyReduction_155 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_4 of { happy_var_4 -> 
	happyIn58
		 (New happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_156 = happyReduce 6# 54# happyReduction_156
happyReduction_156 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (NewLocal happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_157 = happyReduce 6# 54# happyReduction_157
happyReduction_157 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (SyncMethCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_158 = happyReduce 6# 54# happyReduction_158
happyReduction_158 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (ThisSyncMethCall happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_159 = happyReduce 6# 54# happyReduction_159
happyReduction_159 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (AsyncMethCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_160 = happyReduce 6# 54# happyReduction_160
happyReduction_160 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (ThisAsyncMethCall happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_161 = happySpecReduce_3  54# happyReduction_161
happyReduction_161 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (Get happy_var_1
	)}

happyReduce_162 = happyReduce 6# 54# happyReduction_162
happyReduction_162 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut44 happy_x_5 of { happy_var_5 -> 
	happyIn58
		 (Spawns happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyNewToken action sts stk [] =
	happyDoAction 68# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TS _ 61) -> cont 61#;
	PT _ (TS _ 62) -> cont 62#;
	PT _ (TV happy_dollar_dollar) -> cont 63#;
	PT _ (TL happy_dollar_dollar) -> cont 64#;
	PT _ (TI happy_dollar_dollar) -> cont 65#;
	PT _ (T_TypeIdent happy_dollar_dollar) -> cont 66#;
	_ -> cont 67#;
	_ -> happyError' (tk:tks)
	}

happyError_ 68# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut10 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

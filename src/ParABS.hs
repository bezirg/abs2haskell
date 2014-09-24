{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParABS where
import AbsABS
import LexABS
import ErrM
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
happyIn8 :: (Type) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Type)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([AnnType]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([AnnType])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (AnnType) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (AnnType)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (QualType) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (QualType)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([QualType]) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([QualType])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (QualTypeIdent) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (QualTypeIdent)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([QualTypeIdent]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([QualTypeIdent])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Program) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Program)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (ModuleDecl) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (ModuleDecl)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Export) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Export)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ([Export]) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ([Export])
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (ImportType) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (ImportType)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Import) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Import)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([Import]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([Import])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (AnyIdent) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (AnyIdent)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([AnyIdent]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([AnyIdent])
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (AnnDecl) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (AnnDecl)
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
happyIn31 :: (MethSig) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (MethSig)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([MethSig]) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([MethSig])
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (BodyDecl) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (BodyDecl)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([BodyDecl]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([BodyDecl])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Block) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Block)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (MaybeBlock) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (MaybeBlock)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (FunBody) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (FunBody)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([AnnDecl]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([AnnDecl])
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([Stm]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([Stm])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Param) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Param)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([Param]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([Param])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (PureExp) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (PureExp)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (CaseBranch) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (CaseBranch)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([CaseBranch]) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([CaseBranch])
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Pattern) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Pattern)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ([Pattern]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ([Pattern])
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Stm) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Stm)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Guard) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Guard)
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
happyIn53 :: (PureExp) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (PureExp)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (PureExp) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (PureExp)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (PureExp) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (PureExp)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (PureExp) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (PureExp)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (Literal) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (Literal)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (PureExp) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (PureExp)
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (EffExp) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (EffExp)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: ([PureExp]) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> ([PureExp])
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: (Exp) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> (Exp)
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: (Ann) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> (Ann)
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: ([Ann]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> ([Ann])
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x10\x03\xfe\x02\x00\x00\xf0\x02\x00\x00\xef\x02\x00\x00\x24\x03\x23\x03\x00\x00\x00\x00\xea\x02\x00\x00\x08\x03\x00\x00\x1a\x03\x16\x01\x1f\x00\x00\x00\x00\x00\x1d\x03\xf7\x02\xf5\x02\x0d\x00\x0f\x03\xdf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x07\x00\x00\x00\x00\xf3\x02\xed\x02\xc3\x02\xc3\x02\xaf\x00\x00\x00\x00\x00\x00\x00\xc3\x02\xc3\x02\x75\x00\x00\x00\x00\x00\x5c\x02\xc3\x02\xc3\x02\x0c\x03\xc3\x02\xc3\x02\xf9\x02\x6e\x00\xc5\x02\xf8\x02\xe9\x02\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x14\x00\x23\x00\x00\x00\x00\x00\xf1\x02\xd9\x02\xb3\x02\xeb\x02\x38\x00\x27\x03\xb3\x00\x7c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x02\x70\x02\x5c\x02\x5c\x02\xe8\x02\x00\x00\xe0\x02\x00\x00\x70\x02\x00\x00\x00\x00\x56\x00\xa8\x02\x27\x00\x62\x00\x00\x00\xdc\x02\x5c\x02\x4f\x02\x29\x02\xc0\x02\xba\x01\xdb\x02\xd8\x02\xfb\x00\xdd\x02\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x0b\x02\x9f\x02\x9f\x02\x00\x00\x00\x00\x62\x00\x00\x00\xae\x02\xcd\x00\xd6\x02\x0c\x03\x97\x02\x0b\x02\x2e\x01\x00\x00\x10\x01\xa5\x02\xa2\x02\x00\x00\x86\x02\xe3\xff\x0c\x03\xad\x01\x9a\x02\xbc\x00\xad\x01\x87\x01\x69\x01\x7c\x01\x84\x02\x00\x00\x71\x02\x7c\x01\x9c\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x70\x02\x00\x00\x4b\x05\x9d\x00\x00\x00\x67\x02\x67\x02\x0c\x03\xfa\x00\x9d\x00\x00\x00\x0c\x03\x8d\x02\xa4\x08\x90\x02\x5e\x02\x4b\x05\x53\x02\x83\x02\x82\x02\x50\x02\x80\x02\x7a\x02\x72\x02\x44\x05\x44\x02\x3e\x02\x74\x02\x79\x02\x0d\x02\x77\x02\x38\x00\x27\x03\x27\x03\xb3\x00\xb3\x00\xb3\x00\xb3\x00\x7c\x01\x7c\x01\x00\x00\x00\x00\x00\x00\xb6\x02\x00\x00\x6e\x02\x00\x00\x6d\x02\x58\x02\x65\x02\x6b\x02\x00\x00\x5c\x01\x5d\x02\x60\x02\x61\x02\x52\x02\x4c\x02\x00\x00\x45\x02\x00\x00\x14\x02\xfe\x01\x00\x00\x00\x00\x41\x02\xd8\x01\x40\x02\x36\x01\xfb\x01\x00\x00\x0a\x00\x37\x02\x32\x02\x00\x00\xaa\x00\x36\x01\x36\x01\x1a\x01\x2f\x02\x2c\x02\xf4\x00\x08\x02\xff\xff\x2d\x02\x1a\x02\xf4\x00\xf4\x00\xf4\x00\x00\x00\x17\x02\x00\x00\x15\x02\x00\x00\x00\x00\x00\x00\xc9\x00\x11\x02\xa8\x08\x00\x00\x4b\x00\x11\x05\x00\x00\x00\x00\xde\x01\x00\x00\x00\x00\x0f\x02\xd3\x01\x00\x00\xd3\x01\x0b\x00\x00\x00\x04\x02\x0e\x02\x00\x00\x9d\x00\xd4\x01\xfd\x01\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x99\x08\xf9\x01\xfc\x01\xc2\x01\xf7\x01\xf4\x01\x00\x00\x00\x00\x99\x01\x8e\x08\x95\x00\x47\x02\x00\x00\x9d\x00\x00\x00\x00\x00\xf1\x01\xe7\x01\xd9\x01\x00\x00\x00\x00\x00\x00\x00\x00\xd8\x01\x00\x00\x00\x00\x00\x00\xe1\x01\xdd\x01\xdb\x01\x00\x00\xbb\x01\xd8\x01\xd5\x01\x00\x00\xcc\x01\xd2\x01\xce\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x00\x00\xa2\x01\xd8\x01\x00\x00\xac\x08\x47\x02\x8a\x01\x00\x00\x8e\x02\x00\x00\xd8\x01\x00\x00\xc1\x01\x00\x00\x0b\x00\x00\x00\x8e\x02\xd6\x00\x00\x00\x8e\x02\xbf\x01\xab\x01\x00\x00\x00\x00\xaf\x01\x00\x00\x00\x00\xa3\x01\xa9\x01\xf6\x01\x00\x00\x00\x00\x9d\x01\x9f\x01\x00\x00\xd8\x01\x00\x00\x00\x00\x00\x00\xac\x08\xa5\x01\x12\x01\x00\x00\x5e\x01\x00\x00\x93\x01\x00\x00\x00\x00\xd6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x01\x8d\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x01\x7d\x01\x15\x01\x00\x00\x00\x00\x78\x00\x66\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x02\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\x74\x01\x00\x00\x00\x00\x00\x00\x8b\x04\x7a\x04\xc4\x01\x00\x00\x00\x00\x00\x00\x30\x04\x1f\x04\x1b\x05\x00\x00\x00\x00\x6f\x07\x90\x01\x8e\x01\x48\x09\x8b\x01\x66\x01\x00\x00\x00\x00\x88\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x07\x6e\x08\x27\x07\x16\x07\x00\x00\x00\x00\x00\x00\x00\x00\x63\x08\x00\x00\x00\x00\x00\x00\x7e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x05\x07\xc3\x05\xce\x06\x40\x09\x2a\x03\x00\x00\x00\x00\x00\x00\x00\x00\x63\x01\x00\x00\x00\x00\x00\x00\x1b\x05\xbd\x06\x77\x01\x76\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x09\x00\x00\xac\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x01\x73\x01\x30\x09\x86\x04\x4f\x00\x00\x00\x75\x04\x18\x03\x3c\x04\x00\x00\x68\x01\x67\x00\x00\x00\x00\x00\x00\x00\x84\x08\x79\x08\x97\x04\x2c\x08\x7a\x05\x21\x08\x16\x08\x0b\x08\xd4\x07\xc9\x07\xbe\x07\xb3\x07\x7c\x07\x00\x00\x0d\x01\xd5\x03\x46\x01\x53\x00\xee\x00\xce\x08\x00\x00\xc4\x03\x47\x01\xc3\x08\x00\x00\xdd\x04\x00\x00\x00\x00\xf2\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x5b\x01\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x01\xb2\x05\x00\x00\x00\x00\x00\x00\x75\x06\x00\x00\x2b\x04\x53\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x05\x1a\x04\xe1\x03\xcc\x02\x00\x00\x00\x00\xd0\x03\x00\x00\xd1\x04\x00\x00\x00\x00\xbf\x03\x86\x03\x75\x03\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x03\x00\x00\xd1\x01\x00\x00\x00\x00\xae\x00\x00\x00\x24\x01\x35\x01\x00\x00\x21\x01\x00\x00\x06\x00\x00\x00\xe8\x00\x26\x09\x00\x00\x00\x00\x00\x00\x17\x01\x7a\x03\x23\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x04\x00\x00\x00\x00\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x48\x01\x00\x00\xf8\x08\x00\x00\x69\x03\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x04\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x06\x00\x00\x80\x01\xf0\x08\x00\x00\x86\x00\x9b\x00\x00\x00\x0b\x06\x00\x00\x00\x00\x00\x00\x1e\x09\x00\x00\x73\x00\xa1\x05\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x51\x00\x3d\x00\x00\x00\x00\x00\x00\x00\xfa\x05\x00\x00\x00\x00\x00\x00\x9c\x00\x48\x01\x36\x03\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x05\x00\x00\xe5\xff\x00\x00\x00\x00\x48\x01\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\xe7\xff\xea\xff\xeb\xff\x00\x00\xe9\xff\xef\xff\xfb\xff\xea\xff\xe1\xff\xdb\xff\xe8\xff\x00\x00\xae\xff\xd7\xff\xd9\xff\xd8\xff\xd6\xff\xe5\xff\xe3\xff\xd7\xff\x00\x00\x52\xff\xdf\xff\xde\xff\xe0\xff\xad\xff\xb2\xff\xe6\xff\x00\x00\xac\xff\xda\xff\x00\x00\x00\x00\xea\xff\xea\xff\xd7\xff\xd5\xff\xe4\xff\xe2\xff\xea\xff\xea\xff\xea\xff\xd4\xff\x51\xff\xea\xff\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\x00\x00\xf9\xff\xf8\xff\xfa\xff\xf6\xff\x00\x00\x00\x00\x6c\xff\x64\xff\x63\xff\x6a\xff\x00\x00\xa3\xff\x84\xff\x82\xff\x7f\xff\x7a\xff\x77\xff\x73\xff\x70\xff\x6d\xff\x68\xff\xea\xff\xea\xff\xea\xff\xea\xff\x00\x00\x67\xff\x66\xff\x65\xff\xea\xff\xfd\xff\xfc\xff\x6c\xff\x00\x00\x6a\xff\x00\x00\xab\xff\x00\x00\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\x00\x00\x00\x00\x66\xff\x00\x00\xac\xff\xb3\xff\xdd\xff\xdc\xff\xea\xff\xea\xff\x00\x00\x00\x00\x8c\xff\x8b\xff\x55\xff\x54\xff\x00\x00\x66\xff\x00\x00\xea\xff\x00\x00\xea\xff\x6c\xff\x87\xff\x00\x00\x66\xff\x00\x00\x97\xff\x00\x00\x00\x00\xea\xff\xea\xff\x52\xff\x00\x00\xea\xff\xea\xff\xea\xff\x72\xff\x00\x00\x52\xff\x00\x00\x71\xff\x00\x00\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\xea\xff\x53\xff\xa9\xff\xea\xff\xb5\xff\xc1\xff\xbe\xff\xea\xff\x00\x00\xea\xff\xba\xff\xea\xff\x00\x00\xea\xff\xed\xff\x00\x00\xa9\xff\xc1\xff\x00\x00\xc8\xff\xbd\xff\x00\x00\xc0\xff\x00\x00\xb1\xff\x00\x00\x00\x00\xa8\xff\x00\x00\xea\xff\x85\xff\x83\xff\x81\xff\x80\xff\x7b\xff\x7c\xff\x7d\xff\x7e\xff\x78\xff\x79\xff\x75\xff\x76\xff\x74\xff\x61\xff\xa1\xff\x00\x00\x6b\xff\x57\xff\x00\x00\x00\x00\x00\x00\x91\xff\xea\xff\x00\x00\xf2\xff\x00\x00\x00\x00\x00\x00\x5a\xff\x00\x00\x8a\xff\x00\x00\xea\xff\x8d\xff\x89\xff\x00\x00\xea\xff\x00\x00\xea\xff\x00\x00\x94\xff\x6b\xff\x00\x00\x00\x00\x96\xff\xea\xff\xea\xff\xea\xff\xea\xff\x6b\xff\x00\x00\xea\xff\x00\x00\xea\xff\x86\xff\x6b\xff\xea\xff\xea\xff\xea\xff\x69\xff\x52\xff\xf4\xff\x00\x00\x6f\xff\x93\xff\x6e\xff\xea\xff\x00\x00\x00\x00\xf0\xff\x00\x00\xa9\xff\xaa\xff\xb5\xff\x00\x00\xb4\xff\xb5\xff\x00\x00\xc1\xff\xd2\xff\xbe\xff\xea\xff\xf7\xff\x00\x00\x00\x00\xba\xff\xea\xff\x00\x00\x00\x00\xce\xff\xd3\xff\xb9\xff\x00\x00\xec\xff\xea\xff\x00\x00\x00\x00\xc6\xff\xc3\xff\x00\x00\xbc\xff\xbf\xff\xbe\xff\xea\xff\x00\x00\xb1\xff\xa7\xff\xea\xff\xb5\xff\x9f\xff\x9d\xff\x00\x00\x00\x00\x9e\xff\x9b\xff\x66\xff\xa4\xff\xea\xff\x56\xff\x90\xff\xf1\xff\x00\x00\x00\x00\x00\x00\x88\xff\x8f\xff\xea\xff\x00\x00\x60\xff\x00\x00\x00\x00\x00\x00\x95\xff\x5b\xff\x5d\xff\x92\xff\x5f\xff\xa5\xff\xea\xff\x5c\xff\x5e\xff\x59\xff\x00\x00\xea\xff\xa0\xff\x9a\xff\xb1\xff\x00\x00\xb5\xff\xa9\xff\xb8\xff\xea\xff\xcc\xff\x00\x00\xc7\xff\xea\xff\xc5\xff\xa9\xff\xea\xff\xcd\xff\xa9\xff\x00\x00\x00\x00\xaf\xff\xb0\xff\x00\x00\xc2\xff\xd1\xff\x00\x00\x00\x00\xea\xff\xb5\xff\xb5\xff\x99\xff\x00\x00\xa2\xff\xea\xff\x8e\xff\xa6\xff\x9c\xff\x9a\xff\xea\xff\xb1\xff\xca\xff\x00\x00\xb7\xff\x00\x00\xd0\xff\xbb\xff\xea\xff\xb6\xff\xb5\xff\xcb\xff\x98\xff\xea\xff\x00\x00\xcf\xff\xc9\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x03\x00\x1e\x00\x05\x00\x06\x00\x15\x00\x08\x00\x09\x00\x03\x00\x0b\x00\x03\x00\x0d\x00\x2a\x00\x0f\x00\x10\x00\x06\x00\x12\x00\x07\x00\x14\x00\x15\x00\x08\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x06\x00\x11\x00\x1d\x00\x1e\x00\x1f\x00\x19\x00\x21\x00\x19\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x43\x00\x08\x00\x1d\x00\x06\x00\x2b\x00\x3a\x00\x0f\x00\x06\x00\x11\x00\x30\x00\x05\x00\x06\x00\x33\x00\x34\x00\x35\x00\x36\x00\x0f\x00\x38\x00\x39\x00\x3a\x00\x02\x00\x3c\x00\x3d\x00\x3e\x00\x1b\x00\x40\x00\x2c\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x06\x00\x1f\x00\x03\x00\x04\x00\x12\x00\x0b\x00\x07\x00\x03\x00\x09\x00\x0a\x00\x43\x00\x46\x00\x3e\x00\x46\x00\x05\x00\x06\x00\x03\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1e\x00\x06\x00\x1d\x00\x1e\x00\x1f\x00\x16\x00\x21\x00\x43\x00\x01\x00\x1a\x00\x46\x00\x3b\x00\x11\x00\x0b\x00\x0c\x00\x43\x00\x2b\x00\x19\x00\x06\x00\x0c\x00\x1e\x00\x30\x00\x1b\x00\x06\x00\x33\x00\x34\x00\x35\x00\x36\x00\x2c\x00\x38\x00\x06\x00\x3a\x00\x06\x00\x3c\x00\x3d\x00\x3e\x00\x3a\x00\x0b\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x0f\x00\x10\x00\x3e\x00\x3b\x00\x24\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x24\x00\x25\x00\x1d\x00\x1e\x00\x1f\x00\x27\x00\x21\x00\x24\x00\x25\x00\x37\x00\x22\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2b\x00\x06\x00\x3b\x00\x0e\x00\x1e\x00\x30\x00\x11\x00\x3b\x00\x33\x00\x34\x00\x35\x00\x36\x00\x3e\x00\x38\x00\x3b\x00\x3a\x00\x06\x00\x3c\x00\x3d\x00\x3e\x00\x06\x00\x0b\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x09\x00\x1e\x00\x0b\x00\x24\x00\x25\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x29\x00\x2a\x00\x1d\x00\x1e\x00\x1f\x00\x0e\x00\x21\x00\x14\x00\x11\x00\x01\x00\x06\x00\x07\x00\x35\x00\x24\x00\x25\x00\x0b\x00\x2b\x00\x3b\x00\x1f\x00\x20\x00\x0c\x00\x30\x00\x3e\x00\x06\x00\x33\x00\x34\x00\x35\x00\x36\x00\x0b\x00\x38\x00\x46\x00\x3a\x00\x1c\x00\x3c\x00\x3d\x00\x3e\x00\x3b\x00\x21\x00\x03\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x03\x00\x43\x00\x3b\x00\x2b\x00\x46\x00\x20\x00\x21\x00\x06\x00\x30\x00\x06\x00\x07\x00\x01\x00\x34\x00\x16\x00\x0b\x00\x06\x00\x2b\x00\x1a\x00\x3a\x00\x16\x00\x3c\x00\x30\x00\x0c\x00\x1a\x00\x0f\x00\x34\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x3a\x00\x00\x00\x3c\x00\x06\x00\x04\x00\x21\x00\x24\x00\x25\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x3e\x00\x0e\x00\x2b\x00\x06\x00\x0f\x00\x0d\x00\x00\x00\x30\x00\x0b\x00\x11\x00\x48\x00\x34\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x3b\x00\x3a\x00\x1d\x00\x3c\x00\x24\x00\x25\x00\x1c\x00\x06\x00\x00\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x21\x00\x06\x00\x07\x00\x28\x00\x1e\x00\x03\x00\x0b\x00\x1e\x00\x2d\x00\x16\x00\x2b\x00\x09\x00\x0a\x00\x3b\x00\x1b\x00\x30\x00\x03\x00\x04\x00\x33\x00\x34\x00\x07\x00\x3e\x00\x09\x00\x0a\x00\x00\x00\x3a\x00\x43\x00\x3c\x00\x21\x00\x46\x00\x00\x00\x28\x00\x00\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x06\x00\x1c\x00\x1e\x00\x1d\x00\x30\x00\x0b\x00\x00\x00\x03\x00\x34\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x06\x00\x3a\x00\x1d\x00\x3c\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x21\x00\x00\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x08\x00\x1c\x00\x23\x00\x2b\x00\x00\x00\x0d\x00\x21\x00\x0e\x00\x30\x00\x06\x00\x03\x00\x33\x00\x34\x00\x03\x00\x0b\x00\x03\x00\x2b\x00\x41\x00\x3a\x00\x23\x00\x3c\x00\x30\x00\x46\x00\x0e\x00\x3e\x00\x34\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x3a\x00\x11\x00\x3c\x00\x07\x00\x0a\x00\x21\x00\x29\x00\x2a\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x07\x00\x0e\x00\x2b\x00\x06\x00\x07\x00\x35\x00\x07\x00\x30\x00\x0b\x00\x0e\x00\x33\x00\x34\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x06\x00\x3a\x00\x1d\x00\x3c\x00\x00\x00\x0b\x00\x07\x00\x03\x00\x3e\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x21\x00\x0e\x00\x2e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x12\x00\x13\x00\x2b\x00\x07\x00\x0e\x00\x21\x00\x07\x00\x30\x00\x06\x00\x46\x00\x25\x00\x34\x00\x07\x00\x0b\x00\x07\x00\x2b\x00\x41\x00\x3a\x00\x07\x00\x3c\x00\x30\x00\x46\x00\x13\x00\x33\x00\x34\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x3a\x00\x0e\x00\x3c\x00\x06\x00\x27\x00\x21\x00\x29\x00\x07\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x0a\x00\x06\x00\x2b\x00\x06\x00\x43\x00\x35\x00\x06\x00\x30\x00\x0b\x00\x11\x00\x0e\x00\x34\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x06\x00\x3a\x00\x1d\x00\x3c\x00\x07\x00\x0b\x00\x43\x00\x14\x00\x46\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x21\x00\x11\x00\x43\x00\x11\x00\x0e\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x2b\x00\x1d\x00\x14\x00\x21\x00\x25\x00\x30\x00\x06\x00\x16\x00\x04\x00\x34\x00\x07\x00\x0b\x00\x06\x00\x2b\x00\x41\x00\x3a\x00\x07\x00\x3c\x00\x30\x00\x46\x00\x06\x00\x43\x00\x34\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x3a\x00\x06\x00\x3c\x00\x07\x00\x00\x00\x21\x00\x06\x00\x03\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x06\x00\x46\x00\x2b\x00\x06\x00\x0f\x00\x43\x00\x06\x00\x30\x00\x0b\x00\x12\x00\x13\x00\x34\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x06\x00\x3a\x00\x1d\x00\x3c\x00\x00\x00\x0b\x00\x07\x00\x03\x00\x0a\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x21\x00\x14\x00\x07\x00\x0e\x00\x1c\x00\x07\x00\x06\x00\x0a\x00\x12\x00\x13\x00\x2b\x00\x0b\x00\x05\x00\x21\x00\x0a\x00\x30\x00\x07\x00\x43\x00\x3e\x00\x34\x00\x0a\x00\x3e\x00\x14\x00\x2b\x00\x06\x00\x3a\x00\x43\x00\x3c\x00\x30\x00\x46\x00\x0e\x00\x3f\x00\x34\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x3a\x00\x14\x00\x3c\x00\x46\x00\x0a\x00\x0e\x00\x3e\x00\x0f\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x07\x00\x34\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x3a\x00\x1d\x00\x3c\x00\x46\x00\x14\x00\x3e\x00\x0e\x00\x0c\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x0e\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x43\x00\x12\x00\x43\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x39\x00\x43\x00\x1c\x00\x07\x00\x46\x00\x09\x00\x0a\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x25\x00\x06\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x43\x00\x06\x00\x2e\x00\x07\x00\x0e\x00\x09\x00\x0a\x00\x0e\x00\x0e\x00\x43\x00\x0c\x00\x37\x00\x06\x00\x39\x00\x05\x00\x31\x00\x26\x00\x40\x00\x3e\x00\x1c\x00\x40\x00\x06\x00\x0f\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x26\x00\x39\x00\x46\x00\x0f\x00\x43\x00\x46\x00\x11\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x29\x00\x39\x00\x00\x00\x01\x00\x02\x00\x03\x00\x29\x00\x0e\x00\x29\x00\x07\x00\x29\x00\x09\x00\x0a\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x0a\x00\x0e\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\x0c\x00\x46\x00\x07\x00\x0e\x00\x09\x00\x0a\x00\x46\x00\x0f\x00\x10\x00\x48\x00\x03\x00\x04\x00\x14\x00\x15\x00\x07\x00\x26\x00\x09\x00\x0a\x00\x43\x00\x32\x00\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x26\x00\x39\x00\x46\x00\x1d\x00\xff\xff\x1f\x00\x20\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\xff\xff\x39\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\x38\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x38\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\x26\x00\x09\x00\x0a\x00\xff\xff\xff\xff\x2b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x26\x00\x1b\x00\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\x03\x00\x09\x00\x0a\x00\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0f\x00\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x0f\x00\x43\x00\xff\xff\xff\xff\x46\x00\xff\xff\xff\xff\x0f\x00\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\x1d\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x3e\x00\x09\x00\x0a\x00\xff\xff\xff\xff\x43\x00\xff\xff\xff\xff\x46\x00\x21\x00\xff\xff\xff\xff\x43\x00\xff\xff\x26\x00\x46\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x26\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x26\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x22\x00\x23\x00\x24\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\x2f\x00\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\x3b\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\x33\x00\x34\x00\x35\x00\x36\x00\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\x33\x00\x34\x00\x35\x00\x36\x00\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x03\x00\x04\x00\xff\xff\x1d\x00\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\x41\x00\xff\xff\x03\x00\x04\x00\xff\xff\x46\x00\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\x41\x00\xff\xff\x34\x00\xff\xff\xff\xff\x46\x00\x34\x00\xff\xff\x3a\x00\xff\xff\x3c\x00\x41\x00\x3a\x00\xff\xff\x3c\x00\x41\x00\x46\x00\x43\x00\x44\x00\x45\x00\x46\x00\x43\x00\x44\x00\x45\x00\x46\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\x1d\x00\xff\xff\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x1f\x00\x20\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\x03\x00\x04\x00\x17\x00\x18\x00\x07\x00\xff\xff\x09\x00\x0a\x00\x03\x00\x04\x00\x17\x00\x18\x00\x07\x00\xff\xff\x09\x00\x0a\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\x03\x00\x04\x00\xff\xff\xff\xff\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x61\xff\x61\xff\x7d\x01\x61\xff\x51\x00\x2e\x00\x61\xff\x61\xff\xb3\x00\x52\x00\xb3\x00\x61\xff\xd8\x00\x61\xff\x61\xff\xeb\x00\x61\xff\xc4\xff\x61\xff\x61\xff\x25\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\xa0\x00\xec\x00\x3f\x00\x62\x00\x63\x00\x20\x01\x53\x00\x0e\x01\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x03\x00\x17\x00\x3f\x00\x89\x00\x64\x00\x2f\x00\xa3\x00\x86\x00\xa4\x00\x55\x00\x33\x01\xd3\x00\x65\x00\x56\x00\x66\x00\x67\x00\x87\x00\x68\x00\x61\xff\x69\x00\x9b\x00\x58\x00\x6a\x00\x6b\x00\x8b\x00\x61\xff\xa1\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x51\x00\x79\x01\x06\x00\x00\x01\x9c\x00\x52\x00\x39\x00\xb0\x00\x08\x00\x09\x00\x03\x00\x0b\x00\xa2\x00\x0b\x00\xd2\x00\xd3\x00\xb3\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x70\x01\x89\x00\x3f\x00\x62\x00\x63\x00\xb1\x00\x53\x00\x03\x00\x83\x00\x54\x01\x0b\x00\xba\x00\x8a\x00\x03\x00\x04\x00\xf5\xff\x64\x00\xb4\x00\xb7\x00\x84\x00\x71\x01\x55\x00\x8b\x00\xb7\x00\x65\x00\x56\x00\x66\x00\x67\x00\x27\x01\x68\x00\xb7\x00\x69\x00\x51\x00\x58\x00\x6a\x00\x6b\x00\x2f\x00\x52\x00\xe8\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x17\x00\x18\x00\x28\x01\xba\x00\xca\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\xb8\x00\x5c\x01\x3f\x00\x62\x00\x63\x00\xa7\x00\x53\x00\xb8\x00\x60\x01\x85\x00\x19\x00\x51\x01\x28\x01\x42\x00\x43\x00\x29\x01\x64\x00\xb7\x00\xba\x00\x52\x01\x65\x01\x55\x00\x53\x01\xba\x00\x65\x00\x56\x00\x66\x00\x67\x00\xa8\x00\x68\x00\xba\x00\x69\x00\x51\x00\x58\x00\x6a\x00\x6b\x00\xb7\x00\x52\x00\x6c\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x95\x00\x4d\x01\x96\x00\xb8\x00\x64\x01\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x68\x01\x7c\x01\x3f\x00\x62\x00\x63\x00\xd1\x00\x53\x00\x1d\x00\xd2\x00\x71\x00\x51\x00\x58\xff\x2c\x01\xb8\x00\x25\x01\x52\x00\x64\x00\xba\x00\x1e\x00\x1f\x00\xe3\x00\x55\x00\xea\xff\x51\x00\x65\x00\x56\x00\x66\x00\x67\x00\x52\x00\x68\x00\x0b\x00\x69\x00\x58\xff\x58\x00\x6a\x00\x6b\x00\xba\x00\x53\x00\xb0\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\xb0\x00\x03\x00\x20\x00\x54\x00\x0b\x00\x60\x01\x53\x00\xb7\x00\x55\x00\x51\x00\x58\xff\x71\x00\x56\x00\xb1\x00\x52\x00\xae\x00\x54\x00\x1f\x01\x57\x00\xb1\x00\x58\x00\x55\x00\x72\x00\xb2\x00\xaf\x00\x56\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x57\x00\x57\x01\x58\x00\xb7\x00\xdc\x00\x53\x00\xb8\x00\x0f\x01\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x22\x00\xdd\x00\x54\x00\x51\x00\xb1\xff\x0f\x00\x17\x01\x55\x00\x52\x00\x10\x00\xb1\xff\x56\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\xba\x00\x57\x00\x3f\x00\x58\x00\xb8\x00\xb9\x00\x19\x01\x89\x00\x23\x01\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x53\x00\x51\x00\x58\xff\x1b\x00\x22\x01\x06\x00\x52\x00\x24\x01\x1c\x00\xde\x00\x54\x00\x08\x00\x0e\x00\xba\x00\x8b\x00\x55\x00\x06\x00\x05\x01\x65\x00\x56\x00\x39\x00\x22\x00\x08\x00\x09\x00\xec\x00\x78\x00\xb1\xff\x58\x00\x53\x00\x0b\x00\xf2\x00\xff\x00\x03\x01\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x54\x00\x51\x00\xaa\x00\xb5\x00\x06\x01\x55\x00\x52\x00\xcb\x00\x36\x00\x56\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x51\x00\x57\x00\x3f\x00\x58\x00\xd6\x00\x52\x00\xd8\x00\xe4\x00\xe5\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x53\x00\x87\x00\x92\x00\x28\x01\x42\x00\x43\x00\x29\x01\x93\x00\x58\xff\x6e\x00\x54\x00\xa5\x00\x94\x00\x53\x00\x0d\x00\x55\x00\x51\x00\x37\x00\x65\x00\x56\x00\x3f\x00\x52\x00\x40\x00\x54\x00\x81\x01\x78\x00\x2d\x00\x58\x00\x55\x00\x0b\x00\x80\x01\x22\x00\x56\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x57\x00\x79\x01\x58\x00\x6f\x01\x70\x01\x53\x00\x68\x01\x69\x01\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x74\x01\x75\x01\x54\x00\x51\x00\x58\xff\x2c\x01\x76\x01\x55\x00\x52\x00\x77\x01\x65\x00\x56\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x51\x00\x78\x00\x3f\x00\x58\x00\x12\x00\x52\x00\x78\x01\x13\x00\x67\x01\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x53\x00\x63\x01\x6c\x01\x28\x01\x42\x00\x43\x00\x29\x01\x41\x01\x14\x00\x28\x00\x54\x00\x42\x01\x43\x01\x53\x00\x44\x01\x55\x00\x51\x00\x0b\x00\x46\x01\x56\x00\x47\x01\x52\x00\x48\x01\x54\x00\x7c\x01\x57\x00\x49\x01\x58\x00\x55\x00\x0b\x00\x4b\x01\x65\x00\x56\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x78\x00\x4c\x01\x58\x00\x4d\x01\x2a\x01\x53\x00\x2b\x01\x56\x01\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x57\x01\x59\x01\x54\x00\x51\x00\x03\x00\x2c\x01\x5c\x01\x55\x00\x52\x00\x5a\x01\x17\x01\x56\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x51\x00\x57\x00\x3f\x00\x58\x00\x1b\x01\x52\x00\x03\x00\x1c\x01\x0b\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x53\x00\x22\x01\x03\x00\x31\x01\x33\x01\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x31\x00\x54\x00\x3f\x00\x52\xff\x53\x00\x3a\x01\x55\x00\x7c\x00\x38\x01\xdc\x00\x56\x00\x3c\x01\x52\x00\xeb\x00\x54\x00\x73\x01\x80\x00\xe9\x00\x58\x00\x55\x00\x0b\x00\xea\x00\x03\x00\x56\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x57\x00\xef\x00\x58\x00\xf1\x00\x12\x00\x53\x00\xf4\x00\x13\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\xf5\x00\x0b\x00\x54\x00\x51\x00\xb1\xff\x03\x00\xf6\x00\x55\x00\x52\x00\x14\x00\x23\x00\x56\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x51\x00\x57\x00\x3f\x00\x58\x00\x12\x00\x52\x00\xf7\x00\x13\x00\xf8\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x53\x00\xf9\x00\xfb\x00\xfc\x00\xfd\x00\xff\x00\x51\x00\xfe\x00\x14\x00\x15\x00\x54\x00\x52\x00\x9d\x00\x53\x00\x03\x01\x55\x00\x02\x01\x03\x00\x05\x01\x56\x00\x0a\x01\x22\x00\x09\x01\x54\x00\x0d\x01\x80\x00\xb1\xff\x58\x00\x55\x00\x0b\x00\x0b\x01\x0c\x01\x56\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x57\x00\x0e\x01\x58\x00\x0b\x00\x12\x01\x16\x01\x11\x01\x52\xff\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\xc9\x00\x56\x00\x52\xff\x52\xff\x52\xff\x52\xff\x52\xff\x57\x00\x52\xff\x58\x00\x0b\x00\x52\xff\xca\x00\xda\x00\xdb\x00\x59\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\xe4\x00\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\x61\xff\x03\x00\x61\xff\x03\x00\x61\xff\x61\xff\x41\x00\x42\x00\x43\x00\x06\x00\xe0\x00\x52\xff\x61\xff\x44\x00\x52\xff\x08\x00\x09\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x61\xff\xe2\x00\x3f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x03\x00\x70\x00\x61\xff\x44\x00\x73\x00\x08\x00\x09\x00\x74\x00\x82\x00\x03\x00\x8d\x00\x61\xff\x8e\x00\x61\xff\x9d\x00\x7a\x00\x74\x00\x9e\x00\x61\xff\x9f\x00\x61\xff\x86\x00\xa5\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x75\x00\x74\x00\x3c\x01\x0b\x00\x87\x00\x03\x00\x0b\x00\xa9\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x75\x00\x2c\x00\xf9\x00\x41\x00\x42\x00\x43\x00\x06\x00\x2d\x00\x23\x00\x26\x00\x44\x00\x27\x00\x08\x00\x09\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x28\x00\x1d\x00\x3f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x12\x00\x0c\x00\x0b\x00\x44\x00\x0d\x00\x08\x00\x09\x00\x0b\x00\x97\x00\x98\x00\xff\xff\x06\x00\x05\x01\x99\x00\x9a\x00\x39\x00\x74\x00\x08\x00\x09\x00\x03\x00\x06\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x75\x00\x74\x00\xce\x00\x0b\x00\x06\x01\x00\x00\x1e\x00\x7a\x01\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x75\x00\x00\x00\x76\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\xab\x00\x4e\x01\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\xab\x00\x18\x01\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\xcc\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\x31\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\x34\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x35\x01\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\xab\x00\xac\x00\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\xab\x00\xb6\x00\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\xcc\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\x36\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x3d\x01\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\x6c\x00\x00\x00\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\x6d\x00\x00\x00\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\xcc\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\x3e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\xed\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\xcd\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\x29\x00\x00\x00\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x06\x00\x08\x00\x09\x00\x00\x00\x2a\x00\x00\x00\x08\x00\x09\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\xcc\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xcc\x00\xcf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\xd4\x00\x5b\x00\x42\x00\x43\x00\x06\x00\x5c\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x08\x00\x09\x00\xc5\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x5b\x00\x42\x00\x43\x00\x06\x00\x5c\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x12\x01\x00\x00\x00\x00\x39\x00\x5e\x00\x08\x00\x09\x00\x00\x00\x00\x00\x6c\x01\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x60\x00\x5e\x00\x13\x01\x00\x00\x00\x00\x00\x00\x38\x01\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x60\x00\x5b\x00\x42\x00\x43\x00\x06\x00\x5c\x00\x00\x00\x00\x00\x5d\x00\x06\x00\x08\x00\x09\x00\x00\x00\x07\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x5b\x00\x42\x00\x43\x00\x06\x00\x5c\x00\x52\xff\x00\x00\x5d\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x52\xff\x52\xff\x52\xff\x52\xff\x52\xff\x00\x00\x52\xff\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x01\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x60\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x60\x00\xb1\xff\x52\xff\x00\x00\x00\x00\x52\xff\x00\x00\x00\x00\x52\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x00\x00\x3f\x00\x52\xff\x52\xff\x52\xff\x52\xff\x52\xff\x00\x00\x52\xff\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x22\x00\x08\x00\x09\x00\x00\x00\x00\x00\xb1\xff\x00\x00\x00\x00\x0b\x00\x7e\x01\x00\x00\x00\x00\x52\xff\x00\x00\x5e\x01\x52\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\xc3\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x7c\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x01\x7c\x00\x42\x00\x43\x00\x06\x00\x5e\x01\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x7d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x6d\x01\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x63\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x6a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x44\x01\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x49\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xef\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\xde\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xe6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x80\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x90\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x31\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x32\x00\x33\x00\x34\x00\xbc\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x35\x00\x00\x00\xbd\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x36\x00\xbe\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x41\x00\x42\x00\x43\x00\x06\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x00\x00\x3f\x00\xc6\x00\x4d\x00\x4e\x00\x4f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x00\x00\x3f\x00\xc7\x00\x4d\x00\x4e\x00\x4f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x2e\x01\x06\x00\xa9\x00\x00\x00\x2e\x01\x39\x00\x00\x00\x08\x00\x09\x00\x00\x00\x54\x01\x00\x00\x06\x00\xaf\x00\x00\x00\x0b\x00\x39\x00\x00\x00\x08\x00\x09\x00\x00\x00\x5b\x01\x00\x00\x56\x00\x00\x00\x00\x00\x0b\x00\x56\x00\x00\x00\x2f\x01\x00\x00\x58\x00\x15\x01\x2f\x01\x00\x00\x58\x00\x30\x01\x0b\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x03\x00\x5a\x00\x5b\x00\x0b\x00\x06\x00\x05\x01\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x06\x00\x05\x01\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x06\x00\x05\x01\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x06\x01\x00\x00\x1e\x00\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x06\x01\x00\x00\x1e\x00\x4f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x06\x01\x00\x00\x1e\x00\x07\x01\x06\x00\x1c\x01\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x06\x00\x1c\x01\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x06\x00\xd5\x00\x1d\x01\x61\x01\x39\x00\x00\x00\x08\x00\x09\x00\x06\x00\xe0\x00\x1d\x01\x1e\x01\x39\x00\x00\x00\x08\x00\x09\x00\x06\x00\x78\x00\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x06\x00\x38\x00\x00\x00\x00\x00\x39\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 174) [
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
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169),
	(170 , happyReduce_170),
	(171 , happyReduce_171),
	(172 , happyReduce_172),
	(173 , happyReduce_173),
	(174 , happyReduce_174)
	]

happy_n_terms = 73 :: Int
happy_n_nonterms = 60 :: Int

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
	 =  happyIn8
		 (TyUnit
	)

happyReduce_6 = happySpecReduce_1  4# happyReduction_6
happyReduction_6 happy_x_1
	 =  happyIn8
		 (TyInt
	)

happyReduce_7 = happySpecReduce_1  4# happyReduction_7
happyReduction_7 happy_x_1
	 =  happyIn8
		 (TyRat
	)

happyReduce_8 = happyReduce 4# 4# happyReduction_8
happyReduction_8 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (TyFut happy_var_3
	) `HappyStk` happyRest}

happyReduce_9 = happySpecReduce_1  4# happyReduction_9
happyReduction_9 happy_x_1
	 =  happyIn8
		 (TyUnderscore
	)

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (TypeVar happy_var_1
	)}

happyReduce_11 = happyReduce 4# 4# happyReduction_11
happyReduction_11 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (ArgType happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_12 = happySpecReduce_0  5# happyReduction_12
happyReduction_12  =  happyIn9
		 ([]
	)

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((:[]) happy_var_1
	)}

happyReduce_14 = happySpecReduce_3  5# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_15 = happySpecReduce_2  6# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (AnnType (reverse happy_var_1) happy_var_2
	)}}

happyReduce_16 = happySpecReduce_1  7# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (QualType happy_var_1
	)}

happyReduce_17 = happySpecReduce_0  8# happyReduction_17
happyReduction_17  =  happyIn12
		 ([]
	)

happyReduce_18 = happySpecReduce_1  8# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ((:[]) happy_var_1
	)}

happyReduce_19 = happySpecReduce_3  8# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_20 = happySpecReduce_1  9# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (QualTypeIdent happy_var_1
	)}

happyReduce_21 = happySpecReduce_0  10# happyReduction_21
happyReduction_21  =  happyIn14
		 ([]
	)

happyReduce_22 = happySpecReduce_1  10# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ((:[]) happy_var_1
	)}

happyReduce_23 = happySpecReduce_3  10# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_24 = happySpecReduce_1  11# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (Prog happy_var_1
	)}

happyReduce_25 = happyReduce 7# 12# happyReduction_25
happyReduction_25 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	case happyOut21 happy_x_5 of { happy_var_5 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	case happyOut36 happy_x_7 of { happy_var_7 -> 
	happyIn16
		 (ModuleDecl happy_var_2 (reverse happy_var_4) (reverse happy_var_5) (reverse happy_var_6) happy_var_7
	) `HappyStk` happyRest}}}}}

happyReduce_26 = happySpecReduce_2  13# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (ExportAny happy_var_2
	)}

happyReduce_27 = happyReduce 4# 13# happyReduction_27
happyReduction_27 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (ExportAnyFrom happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_28 = happySpecReduce_2  13# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  happyIn17
		 (ExportStar
	)

happyReduce_29 = happyReduce 4# 13# happyReduction_29
happyReduction_29 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (ExportStarFrom happy_var_4
	) `HappyStk` happyRest}

happyReduce_30 = happySpecReduce_0  14# happyReduction_30
happyReduction_30  =  happyIn18
		 ([]
	)

happyReduce_31 = happySpecReduce_3  14# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_32 = happySpecReduce_1  15# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn19
		 (ImportHaskell
	)

happyReduce_33 = happySpecReduce_1  15# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn19
		 (ImportABS
	)

happyReduce_34 = happyReduce 4# 16# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (ImportAnyFrom happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_35 = happyReduce 4# 16# happyReduction_35
happyReduction_35 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (ImportStarFrom happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_36 = happySpecReduce_0  17# happyReduction_36
happyReduction_36  =  happyIn21
		 ([]
	)

happyReduce_37 = happySpecReduce_3  17# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_38 = happySpecReduce_1  18# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (AnyIdentI happy_var_1
	)}

happyReduce_39 = happySpecReduce_1  18# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (AnyIdentT happy_var_1
	)}

happyReduce_40 = happySpecReduce_0  19# happyReduction_40
happyReduction_40  =  happyIn23
		 ([]
	)

happyReduce_41 = happySpecReduce_1  19# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ((:[]) happy_var_1
	)}

happyReduce_42 = happySpecReduce_3  19# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_43 = happySpecReduce_2  20# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (AnnDecl (reverse happy_var_1) happy_var_2
	)}}

happyReduce_44 = happyReduce 5# 21# happyReduction_44
happyReduction_44 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut8 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (TypeDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_45 = happyReduce 5# 21# happyReduction_45
happyReduction_45 (happy_x_5 `HappyStk`
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

happyReduce_46 = happyReduce 8# 21# happyReduction_46
happyReduction_46 (happy_x_8 `HappyStk`
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
		 (ParDataDecl happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_47 = happyReduce 9# 21# happyReduction_47
happyReduction_47 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	case happyOut37 happy_x_8 of { happy_var_8 -> 
	happyIn25
		 (Fun happy_var_2 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_48 = happyReduce 12# 21# happyReduction_48
happyReduction_48 (happy_x_12 `HappyStk`
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
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	case happyOut41 happy_x_8 of { happy_var_8 -> 
	case happyOut37 happy_x_11 of { happy_var_11 -> 
	happyIn25
		 (ParFun happy_var_2 happy_var_3 happy_var_5 happy_var_8 happy_var_11
	) `HappyStk` happyRest}}}}}

happyReduce_49 = happyReduce 5# 21# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (InterfDecl happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_50 = happyReduce 7# 21# happyReduction_50
happyReduction_50 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut32 happy_x_6 of { happy_var_6 -> 
	happyIn25
		 (ExtendsDecl happy_var_2 happy_var_4 (reverse happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_51 = happyReduce 7# 21# happyReduction_51
happyReduction_51 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut34 happy_x_4 of { happy_var_4 -> 
	case happyOut36 happy_x_5 of { happy_var_5 -> 
	case happyOut34 happy_x_6 of { happy_var_6 -> 
	happyIn25
		 (ClassDecl happy_var_2 (reverse happy_var_4) happy_var_5 (reverse happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_52 = happyReduce 10# 21# happyReduction_52
happyReduction_52 (happy_x_10 `HappyStk`
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
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	case happyOut34 happy_x_7 of { happy_var_7 -> 
	case happyOut36 happy_x_8 of { happy_var_8 -> 
	case happyOut34 happy_x_9 of { happy_var_9 -> 
	happyIn25
		 (ClassParamDecl happy_var_2 happy_var_4 (reverse happy_var_7) happy_var_8 (reverse happy_var_9)
	) `HappyStk` happyRest}}}}}

happyReduce_53 = happyReduce 9# 21# happyReduction_53
happyReduction_53 (happy_x_9 `HappyStk`
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
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut34 happy_x_6 of { happy_var_6 -> 
	case happyOut36 happy_x_7 of { happy_var_7 -> 
	case happyOut34 happy_x_8 of { happy_var_8 -> 
	happyIn25
		 (ClassImplements happy_var_2 happy_var_4 (reverse happy_var_6) happy_var_7 (reverse happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_54 = happyReduce 12# 21# happyReduction_54
happyReduction_54 (happy_x_12 `HappyStk`
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
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	case happyOut12 happy_x_7 of { happy_var_7 -> 
	case happyOut34 happy_x_9 of { happy_var_9 -> 
	case happyOut36 happy_x_10 of { happy_var_10 -> 
	case happyOut34 happy_x_11 of { happy_var_11 -> 
	happyIn25
		 (ClassParamImplements happy_var_2 happy_var_4 happy_var_7 (reverse happy_var_9) happy_var_10 (reverse happy_var_11)
	) `HappyStk` happyRest}}}}}}

happyReduce_55 = happySpecReduce_1  22# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (UnaryConstr happy_var_1
	)}

happyReduce_56 = happyReduce 4# 22# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (MultConstr happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_57 = happySpecReduce_1  23# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (EmptyConstrType happy_var_1
	)}

happyReduce_58 = happySpecReduce_2  23# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (RecordConstrType happy_var_1 happy_var_2
	)}}

happyReduce_59 = happySpecReduce_0  24# happyReduction_59
happyReduction_59  =  happyIn28
		 ([]
	)

happyReduce_60 = happySpecReduce_1  24# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((:[]) happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  24# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_0  25# happyReduction_62
happyReduction_62  =  happyIn29
		 ([]
	)

happyReduce_63 = happySpecReduce_1  25# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((:[]) happy_var_1
	)}

happyReduce_64 = happySpecReduce_3  25# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_65 = happySpecReduce_0  26# happyReduction_65
happyReduction_65  =  happyIn30
		 ([]
	)

happyReduce_66 = happySpecReduce_1  26# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((:[]) happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  26# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_68 = happyReduce 5# 27# happyReduction_68
happyReduction_68 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (MethSig happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_69 = happySpecReduce_0  28# happyReduction_69
happyReduction_69  =  happyIn32
		 ([]
	)

happyReduce_70 = happySpecReduce_3  28# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_71 = happySpecReduce_3  29# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (FieldDecl happy_var_1 happy_var_2
	)}}

happyReduce_72 = happyReduce 5# 29# happyReduction_72
happyReduction_72 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (FieldDeclAss happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_73 = happyReduce 6# 29# happyReduction_73
happyReduction_73 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_4 of { happy_var_4 -> 
	case happyOut35 happy_x_6 of { happy_var_6 -> 
	happyIn33
		 (MethDecl happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_74 = happySpecReduce_0  30# happyReduction_74
happyReduction_74  =  happyIn34
		 ([]
	)

happyReduce_75 = happySpecReduce_2  30# happyReduction_75
happyReduction_75 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_76 = happySpecReduce_3  31# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (Block (reverse happy_var_2)
	)}

happyReduce_77 = happySpecReduce_1  32# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (JustBlock happy_var_1
	)}

happyReduce_78 = happySpecReduce_0  32# happyReduction_78
happyReduction_78  =  happyIn36
		 (NoBlock
	)

happyReduce_79 = happySpecReduce_1  33# happyReduction_79
happyReduction_79 happy_x_1
	 =  happyIn37
		 (Builtin
	)

happyReduce_80 = happySpecReduce_1  33# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (PureBody happy_var_1
	)}

happyReduce_81 = happySpecReduce_0  34# happyReduction_81
happyReduction_81  =  happyIn38
		 ([]
	)

happyReduce_82 = happySpecReduce_2  34# happyReduction_82
happyReduction_82 happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_83 = happySpecReduce_0  35# happyReduction_83
happyReduction_83  =  happyIn39
		 ([]
	)

happyReduce_84 = happySpecReduce_2  35# happyReduction_84
happyReduction_84 happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn39
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_85 = happySpecReduce_2  36# happyReduction_85
happyReduction_85 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (Par happy_var_1 happy_var_2
	)}}

happyReduce_86 = happySpecReduce_0  37# happyReduction_86
happyReduction_86  =  happyIn41
		 ([]
	)

happyReduce_87 = happySpecReduce_1  37# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_88 = happySpecReduce_3  37# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_89 = happyReduce 8# 38# happyReduction_89
happyReduction_89 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_3 of { happy_var_3 -> 
	case happyOut42 happy_x_6 of { happy_var_6 -> 
	case happyOut42 happy_x_8 of { happy_var_8 -> 
	happyIn42
		 (Let happy_var_3 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_90 = happyReduce 6# 38# happyReduction_90
happyReduction_90 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	case happyOut42 happy_x_6 of { happy_var_6 -> 
	happyIn42
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_91 = happyReduce 5# 38# happyReduction_91
happyReduction_91 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_4 of { happy_var_4 -> 
	happyIn42
		 (Case happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_92 = happySpecReduce_1  38# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_93 = happySpecReduce_3  39# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (CBranch happy_var_1 happy_var_3
	)}}

happyReduce_94 = happySpecReduce_0  40# happyReduction_94
happyReduction_94  =  happyIn44
		 ([]
	)

happyReduce_95 = happySpecReduce_3  40# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_96 = happySpecReduce_1  41# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (PIdent happy_var_1
	)}

happyReduce_97 = happySpecReduce_1  41# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (PLit happy_var_1
	)}

happyReduce_98 = happySpecReduce_1  41# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (PUnaryConstr happy_var_1
	)}

happyReduce_99 = happyReduce 4# 41# happyReduction_99
happyReduction_99 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (PMultConstr happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_100 = happySpecReduce_1  41# happyReduction_100
happyReduction_100 happy_x_1
	 =  happyIn45
		 (PUnderscore
	)

happyReduce_101 = happySpecReduce_0  42# happyReduction_101
happyReduction_101  =  happyIn46
		 ([]
	)

happyReduce_102 = happySpecReduce_1  42# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 ((:[]) happy_var_1
	)}

happyReduce_103 = happySpecReduce_3  42# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_104 = happySpecReduce_2  43# happyReduction_104
happyReduction_104 happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (SExp happy_var_1
	)}

happyReduce_105 = happySpecReduce_3  43# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (SBlock (reverse happy_var_2)
	)}

happyReduce_106 = happyReduce 5# 43# happyReduction_106
happyReduction_106 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_5 of { happy_var_5 -> 
	happyIn47
		 (SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_107 = happySpecReduce_3  43# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (SReturn happy_var_2
	)}

happyReduce_108 = happyReduce 4# 43# happyReduction_108
happyReduction_108 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (SAss happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_109 = happyReduce 6# 43# happyReduction_109
happyReduction_109 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut61 happy_x_5 of { happy_var_5 -> 
	happyIn47
		 (SFieldAss happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_110 = happySpecReduce_3  43# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (SDec happy_var_1 happy_var_2
	)}}

happyReduce_111 = happyReduce 5# 43# happyReduction_111
happyReduction_111 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut61 happy_x_4 of { happy_var_4 -> 
	happyIn47
		 (SDecAss happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_112 = happyReduce 5# 43# happyReduction_112
happyReduction_112 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_5 of { happy_var_5 -> 
	happyIn47
		 (SIf happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_113 = happyReduce 7# 43# happyReduction_113
happyReduction_113 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_5 of { happy_var_5 -> 
	case happyOut47 happy_x_7 of { happy_var_7 -> 
	happyIn47
		 (SIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_114 = happySpecReduce_3  43# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (SAwait happy_var_2
	)}

happyReduce_115 = happySpecReduce_2  43# happyReduction_115
happyReduction_115 happy_x_2
	happy_x_1
	 =  happyIn47
		 (SSuspend
	)

happyReduce_116 = happySpecReduce_2  43# happyReduction_116
happyReduction_116 happy_x_2
	happy_x_1
	 =  happyIn47
		 (SSkip
	)

happyReduce_117 = happySpecReduce_3  43# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_2 of { happy_var_2 -> 
	happyIn47
		 (SAssert happy_var_2
	)}

happyReduce_118 = happySpecReduce_2  44# happyReduction_118
happyReduction_118 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (VarGuard happy_var_1
	)}

happyReduce_119 = happyReduce 4# 44# happyReduction_119
happyReduction_119 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (FieldGuard happy_var_3
	) `HappyStk` happyRest}

happyReduce_120 = happySpecReduce_1  44# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (ExpGuard happy_var_1
	)}

happyReduce_121 = happySpecReduce_3  44# happyReduction_121
happyReduction_121 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (AndGuard happy_var_1 happy_var_3
	)}}

happyReduce_122 = happySpecReduce_3  45# happyReduction_122
happyReduction_122 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (EOr happy_var_1 happy_var_3
	)}}

happyReduce_123 = happySpecReduce_1  45# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_124 = happySpecReduce_3  46# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (EAnd happy_var_1 happy_var_3
	)}}

happyReduce_125 = happySpecReduce_1  46# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_126 = happySpecReduce_3  47# happyReduction_126
happyReduction_126 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (EEq happy_var_1 happy_var_3
	)}}

happyReduce_127 = happySpecReduce_3  47# happyReduction_127
happyReduction_127 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (ENeq happy_var_1 happy_var_3
	)}}

happyReduce_128 = happySpecReduce_1  47# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_129 = happySpecReduce_3  48# happyReduction_129
happyReduction_129 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (ELt happy_var_1 happy_var_3
	)}}

happyReduce_130 = happySpecReduce_3  48# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (ELe happy_var_1 happy_var_3
	)}}

happyReduce_131 = happySpecReduce_3  48# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (EGt happy_var_1 happy_var_3
	)}}

happyReduce_132 = happySpecReduce_3  48# happyReduction_132
happyReduction_132 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (EGe happy_var_1 happy_var_3
	)}}

happyReduce_133 = happySpecReduce_1  48# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_134 = happySpecReduce_3  49# happyReduction_134
happyReduction_134 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (EAdd happy_var_1 happy_var_3
	)}}

happyReduce_135 = happySpecReduce_3  49# happyReduction_135
happyReduction_135 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (ESub happy_var_1 happy_var_3
	)}}

happyReduce_136 = happySpecReduce_1  49# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_137 = happySpecReduce_3  50# happyReduction_137
happyReduction_137 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (EMul happy_var_1 happy_var_3
	)}}

happyReduce_138 = happySpecReduce_3  50# happyReduction_138
happyReduction_138 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (EDiv happy_var_1 happy_var_3
	)}}

happyReduce_139 = happySpecReduce_3  50# happyReduction_139
happyReduction_139 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (EMod happy_var_1 happy_var_3
	)}}

happyReduce_140 = happySpecReduce_1  50# happyReduction_140
happyReduction_140 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_141 = happySpecReduce_2  51# happyReduction_141
happyReduction_141 happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 (ELogNeg happy_var_2
	)}

happyReduce_142 = happySpecReduce_2  51# happyReduction_142
happyReduction_142 happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 (EIntNeg happy_var_2
	)}

happyReduce_143 = happySpecReduce_1  51# happyReduction_143
happyReduction_143 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_144 = happyReduce 4# 52# happyReduction_144
happyReduction_144 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_145 = happyReduce 4# 52# happyReduction_145
happyReduction_145 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (ENaryCall happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_146 = happySpecReduce_1  52# happyReduction_146
happyReduction_146 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (ELit happy_var_1
	)}

happyReduce_147 = happySpecReduce_1  52# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (EVar happy_var_1
	)}

happyReduce_148 = happySpecReduce_3  52# happyReduction_148
happyReduction_148 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (EThis happy_var_3
	)}

happyReduce_149 = happySpecReduce_1  52# happyReduction_149
happyReduction_149 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (EUnaryConstr happy_var_1
	)}

happyReduce_150 = happyReduce 4# 52# happyReduction_150
happyReduction_150 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (EMultConstr happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_151 = happySpecReduce_1  52# happyReduction_151
happyReduction_151 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (happy_var_1
	)}

happyReduce_152 = happySpecReduce_1  53# happyReduction_152
happyReduction_152 happy_x_1
	 =  happyIn57
		 (LNull
	)

happyReduce_153 = happySpecReduce_1  53# happyReduction_153
happyReduction_153 happy_x_1
	 =  happyIn57
		 (LThis
	)

happyReduce_154 = happySpecReduce_1  53# happyReduction_154
happyReduction_154 happy_x_1
	 =  happyIn57
		 (LWhereAmI
	)

happyReduce_155 = happySpecReduce_1  53# happyReduction_155
happyReduction_155 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (LStr happy_var_1
	)}

happyReduce_156 = happySpecReduce_1  53# happyReduction_156
happyReduction_156 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (LInt happy_var_1
	)}

happyReduce_157 = happySpecReduce_3  54# happyReduction_157
happyReduction_157 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (happy_var_2
	)}

happyReduce_158 = happySpecReduce_3  54# happyReduction_158
happyReduction_158 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (happy_var_2
	)}

happyReduce_159 = happyReduce 5# 55# happyReduction_159
happyReduction_159 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut60 happy_x_4 of { happy_var_4 -> 
	happyIn59
		 (New happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_160 = happyReduce 6# 55# happyReduction_160
happyReduction_160 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	happyIn59
		 (NewLocal happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_161 = happyReduce 6# 55# happyReduction_161
happyReduction_161 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	happyIn59
		 (SyncCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_162 = happyReduce 6# 55# happyReduction_162
happyReduction_162 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	happyIn59
		 (ThisSyncCall happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_163 = happyReduce 6# 55# happyReduction_163
happyReduction_163 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	happyIn59
		 (AsyncCall happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_164 = happyReduce 6# 55# happyReduction_164
happyReduction_164 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	happyIn59
		 (ThisAsyncCall happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_165 = happySpecReduce_3  55# happyReduction_165
happyReduction_165 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (Get happy_var_1
	)}

happyReduce_166 = happyReduce 6# 55# happyReduction_166
happyReduction_166 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut60 happy_x_5 of { happy_var_5 -> 
	happyIn59
		 (Spawn happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_167 = happySpecReduce_0  56# happyReduction_167
happyReduction_167  =  happyIn60
		 ([]
	)

happyReduce_168 = happySpecReduce_1  56# happyReduction_168
happyReduction_168 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((:[]) happy_var_1
	)}

happyReduce_169 = happySpecReduce_3  56# happyReduction_169
happyReduction_169 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	happyIn60
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_170 = happySpecReduce_1  57# happyReduction_170
happyReduction_170 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 (ExpP happy_var_1
	)}

happyReduce_171 = happySpecReduce_1  57# happyReduction_171
happyReduction_171 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 (ExpE happy_var_1
	)}

happyReduce_172 = happySpecReduce_3  58# happyReduction_172
happyReduction_172 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_2 of { happy_var_2 -> 
	happyIn62
		 (SimpleAnn happy_var_2
	)}

happyReduce_173 = happySpecReduce_0  59# happyReduction_173
happyReduction_173  =  happyIn63
		 ([]
	)

happyReduce_174 = happySpecReduce_2  59# happyReduction_174
happyReduction_174 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_2 of { happy_var_2 -> 
	happyIn63
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 72# notHappyAtAll action sts stk []

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
	PT _ (TS _ 63) -> cont 63#;
	PT _ (TS _ 64) -> cont 64#;
	PT _ (TS _ 65) -> cont 65#;
	PT _ (TS _ 66) -> cont 66#;
	PT _ (TV happy_dollar_dollar) -> cont 67#;
	PT _ (TL happy_dollar_dollar) -> cont 68#;
	PT _ (TI happy_dollar_dollar) -> cont 69#;
	PT _ (T_TypeIdent happy_dollar_dollar) -> cont 70#;
	_ -> cont 71#;
	_ -> happyError' (tk:tks)
	}

happyError_ 72# tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut15 x))

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

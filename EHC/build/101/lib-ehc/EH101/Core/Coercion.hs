module EH101.Core.Coercion
( Coe, Coe' (..)
, LRCoeKind (..), lrcoeKindOfCoe
, LRCoe (..), emptyLRCoe
, lrcoeIsId
, mkLRCoe
, mkIdLRCoeWith
, lrcoeLSingleton, lrcoeRSingleton, lrcoeLFromList, lrcoeRFromList
, lrcoeUnion )
where
import EH101.Base.Common
import EH101.Opts.Base
import EH101.Ty
import EH101.Core
import EH101.AbstractCore
import qualified Data.Map as Map
import qualified Data.Set as Set

{-# LINE 41 "src/ehc/Core/Coercion.chs" #-}
type Coe = Coe' CExpr CMetaVal CBind CBound Ty

{-# LINE 53 "src/ehc/Core/Coercion.chs" #-}
data LRCoeKind = LRCoeId | LRCoeOther deriving Eq

lrcoeKindAnd :: LRCoeKind -> LRCoeKind -> LRCoeKind
lrcoeKindAnd LRCoeId LRCoeId = LRCoeId
lrcoeKindAnd _       _       = LRCoeOther

lrcoeKindOfCoe :: Coe -> LRCoeKind
lrcoeKindOfCoe c = if acoreCoeIsId c then LRCoeId else LRCoeOther

{-# LINE 64 "src/ehc/Core/Coercion.chs" #-}
data LRCoe
  = LRCoe
      { lrcoeKind		:: LRCoeKind
      , lrcoeLeftL		:: [Coe]
      , lrcoeRightL 	:: [Coe]
      }

emptyLRCoe :: LRCoe
emptyLRCoe = LRCoe LRCoeId [] []

{-# LINE 76 "src/ehc/Core/Coercion.chs" #-}
lrcoeIsId :: LRCoe -> Bool
lrcoeIsId c = lrcoeKind c == LRCoeId

{-# LINE 81 "src/ehc/Core/Coercion.chs" #-}
mkLRCoe :: Coe -> Coe -> LRCoe
mkLRCoe l r = LRCoe LRCoeOther [l] [r]

mkIdLRCoe' :: Coe -> Coe -> LRCoe
mkIdLRCoe' l r = LRCoe LRCoeId [l] [r]

{-# LINE 89 "src/ehc/Core/Coercion.chs" #-}
mkIdLRCoeWith :: HsName -> CMetaVal -> LRCoe
mkIdLRCoeWith n m = mkIdLRCoe' (acoreCoeAppN [(acoreVar n)]) (acoreCoeLam1 n)

{-# LINE 94 "src/ehc/Core/Coercion.chs" #-}
lrcoeLFromList :: [Coe] -> LRCoe
lrcoeLFromList c = LRCoe LRCoeOther c []

lrcoeRFromList :: [Coe] -> LRCoe
lrcoeRFromList c = LRCoe LRCoeOther [] c

lrcoeLSingleton :: Coe -> LRCoe
lrcoeLSingleton c = LRCoe (lrcoeKindOfCoe c) [c] []

lrcoeRSingleton :: Coe -> LRCoe
lrcoeRSingleton c = LRCoe (lrcoeKindOfCoe c) [] [c]

{-# LINE 108 "src/ehc/Core/Coercion.chs" #-}
lrcoeUnion :: LRCoe -> LRCoe -> LRCoe
lrcoeUnion (LRCoe k1 l1 r1) (LRCoe k2 l2 r2) = LRCoe (lrcoeKindAnd k1 k2) (l1 ++ l2) (r1 ++ r2)


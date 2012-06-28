module EH101.VarLookup
( VarLookup (..)
, varlookupMap
, VarLookupFix, varlookupFix
, varlookupFixDel
, VarLookupCmb (..)
, VarLookupBase (..)
, VarLookupCmbFix, varlookupcmbFix )
where
import EH101.Base.Common
import Data.Maybe

{-# LINE 23 "src/ehc/VarLookup.chs" #-}
class VarLookup m k v where
  varlookupWithMetaLev :: MetaLev -> k -> m -> Maybe v
  varlookup :: k -> m -> Maybe v

  -- defaults
  varlookup = varlookupWithMetaLev 0

{-# LINE 32 "src/ehc/VarLookup.chs" #-}
instance (VarLookup m1 k v,VarLookup m2 k v) => VarLookup (m1,m2) k v where
  varlookupWithMetaLev l k (m1,m2)
    = case varlookupWithMetaLev l k m1 of
        r@(Just _) -> r
        _          -> varlookupWithMetaLev l k m2

instance VarLookup m k v => VarLookup [m] k v where
  varlookupWithMetaLev l k ms = listToMaybe $ catMaybes $ map (varlookupWithMetaLev l k) ms

{-# LINE 43 "src/ehc/VarLookup.chs" #-}
varlookupMap :: VarLookup m k v => (v -> Maybe res) -> k -> m -> Maybe res
varlookupMap get k m
  = do { v <- varlookup k m
       ; get v
       }

{-# LINE 55 "src/ehc/VarLookup.chs" #-}
type VarLookupFix k v = k -> Maybe v

-- | fix looking up to be for a certain var mapping
varlookupFix :: VarLookup m k v => m -> VarLookupFix k v
varlookupFix m = \k -> varlookup k m

{-# LINE 63 "src/ehc/VarLookup.chs" #-}
-- | simulate deletion
varlookupFixDel :: Ord k => [k] -> VarLookupFix k v -> VarLookupFix k v
varlookupFixDel ks f = \k -> if k `elem` ks then Nothing else f k

{-# LINE 80 "src/ehc/VarLookup.chs" #-}
infixr 7 |+>

{-# LINE 84 "src/ehc/VarLookup.chs" #-}
class VarLookupCmb m1 m2 where
  (|+>) :: m1 -> m2 -> m2

{-# LINE 89 "src/ehc/VarLookup.chs" #-}
instance VarLookupCmb m1 m2 => VarLookupCmb m1 [m2] where
  m1 |+> (m2:m2s) = (m1 |+> m2) : m2s

instance (VarLookupCmb m1 m1, VarLookupCmb m1 m2) => VarLookupCmb [m1] [m2] where
  m1 |+> (m2:m2s) = (foldr1 (|+>) m1 |+> m2) : m2s

{-# LINE 105 "src/ehc/VarLookup.chs" #-}
class VarLookupBase m k v | m -> k v where
  varlookupEmpty :: m
  -- varlookupTyUnit :: k -> v -> m

{-# LINE 115 "src/ehc/VarLookup.chs" #-}
type VarLookupCmbFix m1 m2 = m1 -> m2 -> m2

-- | fix combining up to be for a certain var mapping
varlookupcmbFix :: VarLookupCmb m1 m2 => VarLookupCmbFix m1 m2
varlookupcmbFix m1 m2 = m1 |+> m2


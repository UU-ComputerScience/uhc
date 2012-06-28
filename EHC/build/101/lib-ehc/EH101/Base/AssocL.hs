module EH101.Base.AssocL
( Assoc, AssocL
, assocLMapElt, assocLMapKey
, assocLElts, assocLKeys
, assocLGroupSort
, assocLMapUnzip
, ppAssocL, ppAssocL', ppAssocLV
, ppCurlysAssocL )
where
import EH.Util.Pretty
import EH.Util.Utils
import Data.List

{-# LINE 23 "src/ehc/Base/AssocL.chs" #-}
type Assoc k v = (k,v)
type AssocL k v = [Assoc k v]

{-# LINE 33 "src/ehc/Base/AssocL.chs" #-}
ppAssocL' :: (PP k, PP v, PP s) => ([PP_Doc] -> PP_Doc) -> s -> AssocL k v -> PP_Doc
ppAssocL' ppL sep al = ppL (map (\(k,v) -> pp k >|< sep >#< pp v) al)

ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL = ppAssocL' (ppBlock "[" "]" ",") ":"

ppAssocLV :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocLV = ppAssocL' vlist ":"

{-# LINE 44 "src/ehc/Base/AssocL.chs" #-}
-- intended for parsing
ppCurlysAssocL :: (k -> PP_Doc) -> (v -> PP_Doc) -> AssocL k v -> PP_Doc
ppCurlysAssocL pk pv = ppCurlysCommasBlock . map (\(k,v) -> pk k >#< "=" >#< pv v)

{-# LINE 50 "src/ehc/Base/AssocL.chs" #-}
assocLMap :: (k -> v -> (k',v')) -> AssocL k v -> AssocL k' v'
assocLMap f = map (uncurry f)
{-# INLINE assocLMap #-}

assocLMapElt :: (v -> v') -> AssocL k v -> AssocL k v'
assocLMapElt f = assocLMap (\k v -> (k,f v))
{-# INLINE assocLMapElt #-}

assocLMapKey :: (k -> k') -> AssocL k v -> AssocL k' v
assocLMapKey f = assocLMap (\k v -> (f k,v))
{-# INLINE assocLMapKey #-}

{-# LINE 64 "src/ehc/Base/AssocL.chs" #-}
assocLMapUnzip :: AssocL k (v1,v2) -> (AssocL k v1,AssocL k v2)
assocLMapUnzip l = unzip [ ((k,v1),(k,v2)) | (k,(v1,v2)) <- l ]

{-# LINE 69 "src/ehc/Base/AssocL.chs" #-}
assocLKeys :: AssocL k v -> [k]
assocLKeys = map fst
{-# INLINE assocLKeys #-}

assocLElts :: AssocL k v -> [v]
assocLElts = map snd
{-# INLINE assocLElts #-}

{-# LINE 79 "src/ehc/Base/AssocL.chs" #-}
assocLGroupSort :: Ord k => AssocL k v -> AssocL k [v]
assocLGroupSort = map (foldr (\(k,v) (_,vs) -> (k,v:vs)) (panic "Base.Common.assocLGroupSort" ,[])) . groupSortOn fst


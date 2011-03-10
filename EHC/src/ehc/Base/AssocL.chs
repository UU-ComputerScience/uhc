%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Association list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.AssocL}
%%]

%%[1 import(EH.Util.Pretty, EH.Util.Utils)
%%]
%%[1 import(Data.List)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AssocL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.AssocL export(Assoc,AssocL)
type Assoc k v = (k,v)
type AssocL k v = [Assoc k v]
%%]

%%[1.ppAssocL export(ppAssocL)
ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL al = ppListSepFill "[ " " ]" ", " (map (\(k,v) -> pp k >|< ":" >|< pp v) al)
%%]

%%[8.ppAssocL -1.ppAssocL export(ppAssocL,ppAssocL',ppAssocLV)
ppAssocL' :: (PP k, PP v, PP s) => ([PP_Doc] -> PP_Doc) -> s -> AssocL k v -> PP_Doc
ppAssocL' ppL sep al = ppL (map (\(k,v) -> pp k >|< sep >#< pp v) al)

ppAssocL :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocL = ppAssocL' (ppBlock "[" "]" ",") ":"

ppAssocLV :: (PP k, PP v) => AssocL k v -> PP_Doc
ppAssocLV = ppAssocL' vlist ":"
%%]

%%[20 export(ppCurlysAssocL)
-- intended for parsing
ppCurlysAssocL :: (k -> PP_Doc) -> (v -> PP_Doc) -> AssocL k v -> PP_Doc
ppCurlysAssocL pk pv = ppCurlysCommasBlock . map (\(k,v) -> pk k >#< "=" >#< pv v)
%%]

%%[1 export(assocLMapElt,assocLMapKey)
assocLMap :: (k -> v -> (k',v')) -> AssocL k v -> AssocL k' v'
assocLMap f = map (uncurry f)
{-# INLINE assocLMap #-}

assocLMapElt :: (v -> v') -> AssocL k v -> AssocL k v'
assocLMapElt f = assocLMap (\k v -> (k,f v))
{-# INLINE assocLMapElt #-}

assocLMapKey :: (k -> k') -> AssocL k v -> AssocL k' v
assocLMapKey f = assocLMap (\k v -> (f k,v))
{-# INLINE assocLMapKey #-}
%%]

%%[4 export(assocLMapUnzip)
assocLMapUnzip :: AssocL k (v1,v2) -> (AssocL k v1,AssocL k v2)
assocLMapUnzip l = unzip [ ((k,v1),(k,v2)) | (k,(v1,v2)) <- l ]
%%]

%%[1 export(assocLElts,assocLKeys)
assocLKeys :: AssocL k v -> [k]
assocLKeys = map fst
{-# INLINE assocLKeys #-}

assocLElts :: AssocL k v -> [v]
assocLElts = map snd
{-# INLINE assocLElts #-}
%%]

%%[1 export(assocLGroupSort)
assocLGroupSort :: Ord k => AssocL k v -> AssocL k [v]
assocLGroupSort = map (foldr (\(k,v) (_,vs) -> (k,v:vs)) (panic "Base.Common.assocLGroupSort" ,[])) . groupSortOn fst
%%]


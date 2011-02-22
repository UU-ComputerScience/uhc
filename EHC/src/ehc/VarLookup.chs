%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction for looking up something for a variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast) module {%{EH}VarLookup} import({%{EH}Base.Common})
%%]

%%[(6 hmtyinfer || hmtyast) import(Data.Maybe)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
VarLookup abstracts from a Map.
The purpose is to be able to combine maps only for the purpose of searching without actually merging the maps.
This then avoids the later need to unmerge such mergings.
The class interface serves to hide this.
%%]

%%[(6 hmtyinfer || hmtyast) export(VarLookup(..))
class VarLookup m k v where
  varlookupWithMetaLev :: MetaLev -> k -> m -> Maybe v
  varlookup :: k -> m -> Maybe v
  
  -- defaults
  varlookup = varlookupWithMetaLev 0
%%]

%%[(6 hmtyinfer || hmtyast)
instance (VarLookup m1 k v,VarLookup m2 k v) => VarLookup (m1,m2) k v where
  varlookupWithMetaLev l k (m1,m2)
    = case varlookupWithMetaLev l k m1 of
        r@(Just _) -> r
        _          -> varlookupWithMetaLev l k m2

instance VarLookup m k v => VarLookup [m] k v where
  varlookupWithMetaLev l k ms = listToMaybe $ catMaybes $ map (varlookupWithMetaLev l k) ms
%%]

%%[(6 hmtyinfer || hmtyast) export(varlookupMap)
varlookupMap :: VarLookup m k v => (v -> Maybe res) -> k -> m -> Maybe res
varlookupMap get k m
  = do { v <- varlookup k m
       ; get v
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarLookupCmb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
VarLookupCmb abstracts the 'combining' of/from a substitution.
The interface goes along with VarLookup but is split off to avoid functional dependency restrictions.
The purpose is to be able to combine maps only for the purpose of searching without actually merging the maps.
This then avoids the later need to unmerge such mergings.
%%]

%%[(6 hmtyinfer || hmtyast)
infixr 7 |+>
%%]

%%[(6 hmtyinfer || hmtyast) export(VarLookupCmb(..))
class VarLookupCmb m k v | m -> k v where
  (|+>) :: m -> m -> m
%%]

%%[(6 hmtyinfer || hmtyast)
instance (VarLookupCmb m1 k v,VarLookupCmb m2 k v) => VarLookupCmb (m1,m2) k v where
  (m1,m2) |+> (n1,n2)
    = (m1 |+> n1, m2 |+> n2)

instance VarLookupCmb m k v => VarLookupCmb [m] k v where
  (|+>) = zipWith (|+>)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction for looking up something for a variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) module {%{EH}VarLookup} import({%{EH}Base.Common})
%%]

%%[(9 hmtyinfer || hmtyast) import(Data.Maybe)
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

%%[(9 hmtyinfer || hmtyast)
infixr 7 |+>
%%]

%%[(9 hmtyinfer || hmtyast) export(VarLookup(..))
class Ord k => VarLookup m k v | m -> v k where
  varlookup :: k -> m -> Maybe v
  (|+>) :: m -> m -> m
%%]

%%[(9 hmtyinfer || hmtyast)
instance (VarLookup m1 k v,VarLookup m2 k v) => VarLookup (m1,m2) k v where
  varlookup k (m1,m2)
    = case varlookup k m1 of
        r@(Just _) -> r
        _          -> varlookup k m2
  (m1,m2) |+> (n1,n2)
    = (m1 |+> n1, m2 |+> n2)

instance VarLookup m k v => VarLookup [m] k v where
  varlookup k ms = listToMaybe $ catMaybes $ map (varlookup k) ms
  (|+>) = zipWith (|+>)
%%]

%%[(9 hmtyinfer || hmtyast) export(varlookupMap)
varlookupMap :: VarLookup m k v => (v -> Maybe res) -> k -> m -> Maybe res
varlookupMap get k m
  = do { v <- varlookup k m
       ; get v
       }
%%]


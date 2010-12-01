%%[(8 codegen grin) module {%{EH}GrinCode.CommonCrossModule}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cross-module numbering                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) import( {%{EH}GrinCode.Common}, {%{EH}Base.HsName} )
%%]
%%[(8 codegen grin) import( Data.List(sortBy, find), Data.Ord(comparing), Control.Monad(foldM) )
%%]
%%[(8 codegen grin) hs import(EH.Util.Utils (panicJust))
%%]
%%[(8 codegen grin) hs import(Data.Typeable(Typeable), Data.Generics(Data), Data.Binary, {%{EH}Base.Serialize}, Control.Monad (ap))
%%]

%%[(8 codegen grin) export(ModEntry(..), ModOffsets, moEntries, moNextOffset, moEmpty, moNewEntry, moAddEntry, moMerge, moLookupMod, moLookupVar, moRenumber)

data ModEntry = ModEntry
  { moModName :: !HsName
  , moLength  :: !Int
  , moOffset  :: !Int
  } deriving (Show, Eq, Data, Typeable)

newtype ModOffsets = ModOffsets [ModEntry]
  deriving (Show, Eq, Data, Typeable)

instance Serialize ModOffsets where
  sget = sgetPlain
  sput = sputPlain
--   sget = return ModOffsets `ap` sget
--   sput = sput . moEntries

instance Binary ModOffsets where
  get = return ModOffsets `ap` get
  put = put . moEntries

-- instance Serialize ModEntry where
--   sget   = return ModEntry `ap` sget `ap` sget `ap` sget
--   sput e = sput (moModName e) >> sput (moLength e) >> sput (moOffset e)

instance Binary ModEntry where
  get   = return ModEntry `ap` get `ap` get `ap` get
  put e = put (moModName e) >> put (moLength e) >> put (moOffset e)

moEntries :: ModOffsets -> [ModEntry]
moEntries (ModOffsets os) = os

moEmpty :: ModOffsets
moEmpty = ModOffsets []

-- | Returns the first unused number, which is the offset a newly added entry
--   would get.
moNextOffset :: ModOffsets -> Int
moNextOffset = (\m -> moOffset m + moLength m) . last . moEntries

moNewEntry :: ModOffsets -> HsName -> Int -> Maybe ModOffsets
moNewEntry mo nm len = moAddEntry mo entry
  where entry = ModEntry
                  { moModName = nm
                  , moLength  = len
                  , moOffset  = 0
                  }

moAddEntry :: ModOffsets -> ModEntry -> Maybe ModOffsets
moAddEntry mo (ModEntry { moModName = nm, moLength = len })
  = fmap ModOffsets . insOffset' firstNonSpecialNr . moEntries $ mo
  where
    insOffset' i []          = Just [ModEntry nm len i]
    insOffset' i offs@(o:os) | moModName o == nm
                               = if   moLength o /= len
                                 then Nothing
                                 else fmap (ModEntry nm len i :) $ insOffset' (i + len) os
                             | otherwise
                               = fmap (o { moOffset = i } :) $ insOffset' (i + moLength o) os

moMerge :: ModOffsets -> ModOffsets -> Maybe ModOffsets
moMerge mo1@(ModOffsets es1) mo2@(ModOffsets es2)
  | length es1 >= length es2 = foldM moAddEntry mo1 es2
  | otherwise                = foldM moAddEntry mo2 es1

moLookupMod :: HsName -> ModOffsets -> Maybe ModEntry
moLookupMod nm = find ((== nm) . moModName) . moEntries

moLookupVar :: Int -> ModOffsets -> Maybe ModEntry
moLookupVar i | i < firstNonSpecialNr = const Nothing
              | otherwise             = find inside . moEntries
              where
                inside o = let j = moOffset o
                           in  i >= j && i < j + moLength o

-- | If the first |ModOffsets| can be renumbered into the second |ModOffsets|
--   (see |moCheckRenumber|), this function returns a renumbering function.
--   If not, the result is |Nothing|.
moRenumber :: ModOffsets -> ModOffsets -> Maybe (Int -> Int)
moRenumber from to
  | from == to                    = Just id
  | not (moCheckRenumber from to) = Nothing
  | otherwise                     = Just renumber
  where
    renumber i
      | i < firstNonSpecialNr = i
      | otherwise             = panicJust "moRenumber" $ do
          ModEntry {moModName = mod, moOffset = ooff} <- moLookupVar i from
          ModEntry {moOffset = noff}                  <- moLookupMod mod to
          return $ i + noff - ooff


-- | Check if the first ModOffsets can be renumbered into the second.
--   For this, all modules in the first ModOffsets have to be present in the
--   second ModOffsets, having the same length.
moCheckRenumber :: ModOffsets -> ModOffsets -> Bool
moCheckRenumber (ModOffsets os1) (ModOffsets os2) = zop os1' os2'
  where os1' = sortBy (comparing moModName) os1
        os2' = sortBy (comparing moModName) os2
        zop [] _  = True
        zop _  [] = False
        zop (x:xs) (y:ys) | moModName x == moModName y = moLength x == moLength y && zop xs ys
                          | otherwise                  = zop (x:xs) ys

%%]


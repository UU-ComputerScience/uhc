%%[(8 codegen grin) module {%{EH}GrinCode.CommonCrossModule}
%%]

%%[(8 codegen grin) import( {%{EH}GrinCode}, {%{EH}GrinCode.Common}, {%{EH}Base.HsName} )
%%]
%%[(8 codegen grin) import( Data.List(sortBy, find, sort, group), Data.Ord(comparing), Control.Monad(foldM) )
%%]
%%[(8 codegen grin) hs import(Debug.Trace, EH.Util.Utils (panicJust))
%%]
%%[(8 codegen grin) hs import(Data.Typeable(Typeable), Data.Generics(Data), Data.Binary, {%{EH}Base.Serialize}, Control.Monad (ap))
%%]
%%[(20 codegen grin) import( {%{EH}GrinCode.Trf.RenumberIdents(renumberIdents)} )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cross-module numbering                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(ModEntry(..), ModOffsets, moEntries, moNextOffset, moEmpty, moNewEntry, moAddEntry, moMerge, moConcat, moLookupMod, moLookupVar, moRenumber)

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
  get = return (moCheck ["Binary.get"] . ModOffsets) `ap` get
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
moNextOffset (ModOffsets []) = firstNonSpecialNr
moNextOffset mo              = (\m -> moOffset m + moLength m) . last . moEntries $ mo

moNewEntry :: ModOffsets -> HsName -> Int -> Maybe ModOffsets
moNewEntry mo nm len = moAddEntry mo entry
  where entry = ModEntry
                  { moModName = nm
                  , moLength  = len
                  , moOffset  = 0
                  }

moAddEntry :: ModOffsets -> ModEntry -> Maybe ModOffsets
moAddEntry mo me@(ModEntry { moModName = nm, moLength = len })
  = fmap (moCheck ["moAddEntry", show mo, show me]) . fmap ModOffsets . insOffset' firstNonSpecialNr . moEntries $ mo
  where
    insOffset' i []          = Just [ModEntry nm len i]
    insOffset' i offs@(o:os) | moModName o == nm
                               = if   moLength o /= len
                                 then Nothing
                                 else Just offs
                             | otherwise
                               = fmap (o:) $ insOffset' (i + moLength o) os

moMerge :: ModOffsets -> ModOffsets -> Maybe ModOffsets
moMerge mo1@(ModOffsets es1) mo2@(ModOffsets es2)
  | length es1 >= length es2 = check $ foldM moAddEntry mo1 es2
  | otherwise                = check $ foldM moAddEntry mo2 es1
  where check = fmap $ moCheck ["moMerge", show mo1, show mo2]

moConcat :: [ModOffsets] -> Maybe ModOffsets
moConcat = foldM moMerge moEmpty

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
      | otherwise             = panicJust info $ do
          ModEntry {moModName = mod, moOffset = ooff} <- moLookupVar i from
          ModEntry {moOffset = noff}                  <- moLookupMod mod to
          return $ i + noff - ooff
      where info = (unwords ["moRenumber", show from, show to, show i])


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

-- | Checks a ModOffsets for consistency
moCheck :: [String] -> ModOffsets -> ModOffsets
%%[[8
moCheck args mo =
  case and $ map ($ moEntries mo) [checkNames, checkOffsets firstNonSpecialNr, checkLengths] of
    False -> error $ "Invalid ModOffsets produced by " ++ unwords args ++ ": " ++ show mo
    True  -> mo
  where checkNames = null . filter ((>1) . length) . group . sort . map moModName
        checkOffsets i []     = True
        checkOffsets i (o:os) = moOffset o == i && checkOffsets (i + moLength o) os
        checkLengths = and . map ((> 0) . moOffset)
%%][100
{-# INLINE moCheck #-}
moCheck _ mo = mo
%%]]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Merging with renumbering                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen grin) hs export(grModMergeRenumber)
grModMergeRenumber :: ModOffsets -> [(GrModule, ModOffsets)] -> GrModule
grModMergeRenumber oTo = grModMerge . map renum
  where renum (mod, oFrom) =
          let f = panicJust "grModMergeRenumber" $ moRenumber oFrom oTo
          in  renumberIdents f mod
%%]


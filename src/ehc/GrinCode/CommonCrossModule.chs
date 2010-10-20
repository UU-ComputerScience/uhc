%%[(8 codegen grin) module {%{EH}GrinCode.CommonCrossModule}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cross-module numbering                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) import( {%{EH}GrinCode.Common}, {%{EH}Base.HsName} )
%%]
%%[(8 codegen grin) import( Data.List(sortBy), Data.Ord(comparing) )
%%]
%%[(8 codegen grin) hs import(EH.Util.Utils (panicJust))
%%]

%%[(8 codegen grin) export(ModOffset(..), ModOffsets, moOffsets, moEmpty, moSetLength, moLookupMod, moLookupVar, moRenumber)

data ModOffset = ModOffset
  { moModName :: HsName
  , moOffset  :: Int
  , moLength  :: Int
  } deriving Show

newtype ModOffsets = ModOffsets [ModOffset]
  deriving Show

moOffsets :: ModOffsets -> [ModOffset]
moOffsets (ModOffsets os) = os

moEmpty :: ModOffsets
moEmpty = ModOffsets []

moSetLength :: HsName -> Int -> ModOffsets -> ModOffsets
moSetLength nm len = ModOffsets . insOffset' firstNonSpecialNr . moOffsets
  where
    insOffset' i []                     = [ModOffset nm i len]
    insOffset' i offs@(o:os) | moModName o == nm = (ModOffset nm i len) : insOffset' (i + len) os
                             | otherwise         = o { moOffset = i } : insOffset' (i + moLength o) os


moLookupMod :: HsName -> ModOffsets -> Maybe ModOffset
moLookupMod nm = moLookupMod' . moOffsets
  where
    moLookupMod' []     = Nothing
    moLookupMod' (o:os) | nm == moModName o = Just o
                        | otherwise         = moLookupMod' os

moLookupVar :: Int -> ModOffsets -> Maybe ModOffset
moLookupVar i = moLookupVar' . moOffsets
  where
    moLookupVar' []     = Nothing
    moLookupVar' (o:os) | i >= j && i < j + moLength o = Just o
                        | otherwise                    = moLookupVar' os
                        where j = moOffset o

-- | If the first |ModOffsets| can be renumbered into the second |ModOffsets|
--   (see |moCheckRenumber|), this function returns a renumbering function.
--   If not, the result is |Nothing|.
moRenumber :: ModOffsets -> ModOffsets -> Maybe (Int -> Int)
moRenumber from to
  | not (moCheck from to) = Nothing
  | otherwise             = Just renumber
  where
    renumber i
      | i < firstNonSpecialNr = i
      | otherwise             = panicJust "moRenumber" $ do
          ModOffset {moModName = mod, moOffset = ooff} <- moLookupVar i from
          ModOffset {moOffset = noff}                  <- moLookupMod mod to
          return $ i + noff - ooff


-- | Check if the first ModOffsets can be renumbered into the second.
--   For this, all modules in the first ModOffsets have to be present in the
--   second ModOffsets, having the same length.
moCheck :: ModOffsets -> ModOffsets -> Bool
moCheck (ModOffsets os1) (ModOffsets os2) = zop os1' os2'
  where os1' = sortBy (comparing moModName) os1
        os2' = sortBy (comparing moModName) os2
        zop [] _  = True
        zop _  [] = False
        zop (x:xs) (y:ys) | moModName x == moModName y = moLength x == moLength y && zop xs ys
                          | otherwise                  = zop (x:xs) ys

%%]


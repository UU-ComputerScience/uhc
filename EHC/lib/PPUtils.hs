module PPUtils where

import UU.Pretty

-------------------------------------------------------------------------
-- PP utils
-------------------------------------------------------------------------

type PP_DocL = [PP_Doc]

ppListSep :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSep o c s pps
  = o >|< l pps >|< c
  where l []      = empty
        l [p]     = pp p
        l (p:ps)  = pp p >|< map (s >|<) ps

ppListSepV' :: (PP s, PP c, PP o, PP a) => (forall x y . (PP x, PP y) => x -> y -> PP_Doc) -> o -> c -> s -> [a] -> PP_Doc
ppListSepV' aside o c s pps
  = l pps
  where l []      = o `aside` c
        l [p]     = o `aside` p `aside` c
        l (p:ps)  = vlist ([o `aside` p] ++ map (s `aside`) (init ps) ++ [s `aside` last ps `aside` c])

ppListSepV :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepV = ppListSepV' (>|<)

ppListSepVV :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepVV = ppListSepV' (>-<)

{-
ppListSepV :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepV o c s pps
  = l pps
  where l []      = o >|< c
        l [p]     = ppPacked o c p
        l (p:ps)  = vlist ([o >|< p] ++ map (s >|<) (init ps) ++ [s >|< last ps >|< c])

ppListSepVV :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepVV o c s pps
  = o >-< foldr (\l r -> l >-< s >-< r) empty pps >-< c
-}

ppPacked :: (PP o, PP c, PP p) => o -> c -> p -> PP_Doc
ppPacked o c pp
  = o >|< pp >|< c

ppParens, ppCurly, ppVBar :: PP p => p -> PP_Doc
ppParens = ppPacked "(" ")"
ppCurly = ppPacked "{" "}"
ppVBar = ppPacked "|" "|"

ppDots :: PP a => [a] -> PP_Doc
ppDots = ppListSep "" "" "."



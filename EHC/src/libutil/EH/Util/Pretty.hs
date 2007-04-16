-------------------------------------------------------------------------
-- Wrapper module around pretty printing
-------------------------------------------------------------------------

module EH.Util.Pretty
  ( -- module UU.Pretty
    -- module EH.Util.Chitil.Pretty
    module EH.Util.PrettySimple
  
  , PP_DocL
  
  , ppListSep, ppListSepV, ppListSepVV
  , ppBlock, ppBlock'
  , ppCommas, ppCommas'
  , ppSpaces
  , ppCurlys
  , ppCurlysBlock
  , ppCurlysSemisBlock
  , ppCurlysCommasBlock
  , ppCurlysCommas, ppCurlysCommas'
  , ppCurlysSemis, ppCurlysSemis'
  , ppParensCommas, ppParensCommas'
  , ppParensSemisBlock
  , ppParensCommasBlock
  , ppBrackets
  , ppBracketsCommas, ppBracketsCommas', ppBracketsCommasV
  , ppHorizontally, ppVertically
  
  , ppPacked, ppParens, ppCurly, ppVBar
  
  , ppDots, ppMb, ppUnless, ppWhen
  
  , hPutWidthPPLn, putWidthPPLn
  , hPutPPLn, putPPLn
  , hPutPPFile, putPPFile
  )
  where

-- import UU.Pretty
-- import EH.Util.Chitil.Pretty
import EH.Util.PrettySimple
import IO
import Data.List

-------------------------------------------------------------------------
-- PP utils for lists
-------------------------------------------------------------------------

type PP_DocL = [PP_Doc]

ppListSep :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSep o c s pps = o >|< hlist (intersperse (pp s) (map pp pps)) >|< c
{-
ppListSep o c s pps
  = o >|< l pps >|< c
  where l []      = empty
        l [p]     = pp p
        l (p:ps)  = pp p >|< map (s >|<) ps
-}

ppBlock' :: PP a => String -> String -> String -> [a] -> [PP_Doc]
ppBlock' o c s []     = [o >|< c]
ppBlock' o c s [a]    = [o >|< a >|< c]
ppBlock' o c s (a:as) = [o >|< a] ++ map (s >|<) as ++ [pp c]

ppBlock :: PP a => String -> String -> String -> [a] -> PP_Doc
ppBlock o c s = vlist . ppBlock' o c s

ppCommas :: PP a => [a] -> PP_Doc
ppCommas = ppListSep "" "" ","

ppCommas' :: PP a => [a] -> PP_Doc
ppCommas' = ppListSep "" "" ", "

ppSpaces :: PP a => [a] -> PP_Doc
ppSpaces = ppListSep "" "" " "

ppCurlysBlock :: PP a => [a] -> PP_Doc
ppCurlysBlock = ppBlock "{ " "}" "  " . map pp

ppCurlysSemisBlock :: PP a => [a] -> PP_Doc
ppCurlysSemisBlock = ppBlock "{ " "}" "; " . map pp

ppCurlysCommasBlock :: PP a => [a] -> PP_Doc
ppCurlysCommasBlock = ppBlock "{ " "}" ", " . map pp

ppParensSemisBlock :: PP a => [a] -> PP_Doc
ppParensSemisBlock = ppBlock "( " ")" "; " . map pp

ppParensCommasBlock :: PP a => [a] -> PP_Doc
ppParensCommasBlock = ppBlock "( " ")" ", " . map pp

ppBracketsCommas :: PP a => [a] -> PP_Doc
ppBracketsCommas = ppListSep "[" "]" ","

ppBracketsCommasV :: PP a => [a] -> PP_Doc
ppBracketsCommasV = ppListSepV3 "[ " "]" ", "

ppBracketsCommas' :: PP a => [a] -> PP_Doc
ppBracketsCommas' = ppListSep "[" "]" ", "

ppParensCommas :: PP a => [a] -> PP_Doc
ppParensCommas = ppListSep "(" ")" ","

ppParensCommas' :: PP a => [a] -> PP_Doc
ppParensCommas' = ppListSep "(" ")" ", "

ppCurlysCommas :: PP a => [a] -> PP_Doc
ppCurlysCommas = ppListSep "{" "}" ","

ppCurlysCommas' :: PP a => [a] -> PP_Doc
ppCurlysCommas' = ppListSep "{" "}" ", "

ppCurlysSemis :: PP a => [a] -> PP_Doc
ppCurlysSemis = ppListSep "{" "}" ";"

ppCurlysSemis' :: PP a => [a] -> PP_Doc
ppCurlysSemis' = ppListSep "{" "}" ", "

{-
ppCommaListV :: PP a => [a] -> PP_Doc
ppCommaListV = ppListSepVV "[" "]" "; "
-}

ppListSepV' :: (PP s, PP c, PP o, PP a) => (forall x y . (PP x, PP y) => x -> y -> PP_Doc) -> o -> c -> s -> [a] -> PP_Doc
ppListSepV' aside o c s pps
  = l pps
  where l []      = o `aside` c
        l [p]     = o `aside` p `aside` c
        l (p:ps)  = vlist ([o `aside` p] ++ map (s `aside`) (init ps) ++ [s `aside` last ps `aside` c])

-- compact vertical list
ppListSepV3 :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSepV3 o c s pps
  = l pps
  where l []      = o >|< c
        l [p]     = o >|< p >|< c
        l (p:ps)  = vlist ([o >|< p] ++ map (s >|<) (init ps) ++ [s >|< last ps >|< c])
{-
-}

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

ppVertically :: [PP_Doc] -> PP_Doc
ppVertically = vlist

ppHorizontally :: [PP_Doc] -> PP_Doc
ppHorizontally = hlist

-------------------------------------------------------------------------
-- Printing open/close pairs
-------------------------------------------------------------------------

ppPacked :: (PP o, PP c, PP p) => o -> c -> p -> PP_Doc
ppPacked o c pp
  = o >|< pp >|< c

ppParens, ppBrackets, ppCurly, ppCurlys, ppVBar :: PP p => p -> PP_Doc
ppParens   = ppPacked "(" ")"
ppBrackets = ppPacked "[" "]"
ppCurly    = ppPacked "{" "}"
ppCurlys   = ppCurly
ppVBar     = ppPacked "|" "| "

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

ppDots :: PP a => [a] -> PP_Doc
ppDots = ppListSep "" "" "."

ppMb :: PP a => Maybe a -> PP_Doc
ppMb = maybe empty pp

ppUnless :: Bool -> PP_Doc -> PP_Doc
ppUnless b p = if b then empty else p

ppWhen :: Bool -> PP_Doc -> PP_Doc
ppWhen b p = if b then p else empty

instance PP a => PP (Maybe a) where
  pp = maybe (pp "?") pp

instance PP Bool where
  pp = pp . show

-------------------------------------------------------------------------
-- PP printing to file
-------------------------------------------------------------------------

hPutLn :: Handle -> Int -> PP_Doc -> IO ()
{-
hPutLn h w pp
  = do hPut h pp w
       hPutStrLn h ""
-}
hPutLn h w pp
  = hPutStrLn h (disp pp w "")

hPutWidthPPLn :: Handle -> Int -> PP_Doc -> IO ()
hPutWidthPPLn h w pp = hPutLn h w pp

putWidthPPLn :: Int -> PP_Doc -> IO ()
putWidthPPLn = hPutWidthPPLn stdout

hPutPPLn :: Handle -> PP_Doc -> IO ()
hPutPPLn h = hPutWidthPPLn h 4000

putPPLn :: PP_Doc -> IO ()
putPPLn = hPutPPLn stdout

hPutPPFile :: Handle -> PP_Doc -> Int -> IO ()
hPutPPFile h pp wid
  = hPutLn h wid pp
    

putPPFile :: String -> PP_Doc -> Int -> IO ()
putPPFile fn pp wid
  =  do  {  h <- openFile fn WriteMode
         ;  hPutPPFile h pp wid
         ;  hClose h
         }

%%[1 hs module (ScannerMachine)
%%]

%%[1 hs export(scan)
%%]

%%[1 hs import (Char(isLower, isUpper, isSpace, isAlphaNum, isDigit, chr, ord))
%%]

%%[1 hs import (List(sort), Maybe(isJust))
%%]

%%[1 hs import (UU.Util.BinaryTrees(tab2tree,btLocateIn))
%%]

%%[1 hs import (UU.Scanner.Token(Token, EnumValToken(..), valueToken, reserved, errToken))
%%]

%%[1 hs import (UU.Scanner.Position(Pos, initPos, advc, adv))
%%]

%%[1 hs import (EH.Util.ScanUtils)
%%]

%%[1 hs

{- A parametrisable scanner
 -
 - Author: Doaitse Swierstra: doaitse@cs.uu.nl
      and: Pablo Azero      : pablo@cs.uu.nl
 - Version 1.0 , May 25, 1998, SDS
    first appearance on the software web site.
 - Version 1.01, June 7, 1998, SDS
    changed String recognition to recognise escaped characters
 - Version 1.02, Aug 30, 1998, SDS
    includes with unsafePerformIO
 - Version 2.1,  Jul  7, 1999, slightly different definition of valueToken
                               ordering between tokens introduced
 - Version 2.2,  Jul  8, 1999, AG_Scanner and UU_Scanner merged
 - Version 2.3,  Jul 15, 1999, modifications: recognize decimal, octal and
 -                             hexadecimal numbers; handles ' as part of a
 -                             lower case identifier
 -                             fixes: bug in msort (loops when passing an
 -                             empty list)
 - Version 2.4,  Jul 23, 1999, additions: recognize characters and infix
 -                             operators
 -
 - Lang. compat: Hugs 98 (because it is required by UU_Parsing)
 - Version 2.5,  Aug 15, 1999, changed names, pSym -> pSpec
                             , all parsers start with p....
 - Version 2.6,  Sept 15, 1999, changed error message for unterminated string
 - Version 2.7,  Sept 23, 1999, changed definition of pOper_Any
 - Version 2.8   Aug 14,  2000, adapted to changes in search trees
 - ??            Oct 25,  2000, adapted to use column numbers
 - ??            Feb 2,   2001, incorporated changes of AD
 - ??            Feb 28,  2001, tabs are handled correctly for column numbers
 - ??            Mar 1,   2001, now generates space tokens that have to be filtered again
 - ??            Apr 4,   2001, tabs are now handled relative to current column number
 - ??            Feb 24,  2006, cloned for use in Ruler
 -}

scan :: ScanOpts -> Pos -> String -> [Token]
scan opts pos input
  = doScan pos input

 where
   locatein :: Ord a => [a] -> a -> Bool
   locatein es = isJust . btLocateIn compare (tab2tree (sort es))

   iskw     = locatein (scoKeywordsTxt opts)
   isop     = locatein (scoKeywordsOps opts)
   isSymbol = locatein (scoSpecChars opts)
   isOpsym  = locatein (scoOpChars opts)


   isIdStart c = isLower c || c == '_'

   isIdChar c =  isAlphaNum c
              || c == '\''
              || c == '_'

   scanFor f p s = let (name,rest) = span f s
                   in (name,advc (length name) p,rest)
   scanIdent     = scanFor isIdChar
   scanDash      = scanFor (== '-')

   doScan p [] = []
   doScan p (c:s)        | isSpace c = let (sp,next) = span isSpace s
                                       in  doScan (foldl adv p (c:sp)) next

   doScan p ('-':'-':'-':'-':s)  = doScan p (dropWhile (/= '\n') s)
   doScan p ('-':'-':'-':s)
     = let (_,p',s') = scanDash (advc 3 p) s
           dash = "---"
           tok | isop dash = reserved dash p
               | otherwise = valueToken TkOp dash p
       in  tok : doScan p' s'
   doScan p ('-':'-':s)  = doScan p (dropWhile (/= '\n') s)
   doScan p ('{':'-':s)  = lexNest doScan (advc 2 p) s
   doScan p ('"':ss)
     = let (s,swidth,rest) = scanString ss
       in if null rest || head rest /= '"'
             then errToken "Unterminated string literal" p : doScan (advc swidth p) rest
             else valueToken TkString s p : doScan (advc (swidth+2) p) (tail rest)

   doScan p ('\'':ss)
     = let (mc,cwidth,rest) = scanChar ss
       in case mc of
            Nothing -> errToken "Error in character literal" p : doScan (advc cwidth p) rest
            Just c  -> if null rest || head rest /= '\''
                          then errToken "Unterminated character literal" p : doScan (advc (cwidth+1) p) rest
                          else valueToken TkChar [c] p : doScan (advc (cwidth+2) p) (tail rest)

   {-
   In Haskell infix identifiers consist of three separate tokens(two backquotes + identifier)
   doScan p ('`':ss)
     = case ss of
         []    -> [errToken "Unterminated infix identifier" p]
         (c:s) -> let res | isIdStart c || isUpper c =
                                   let (name,p1,rest) = scanIdent (advc 2 p) s
                                       ident = c:name
                                       tokens | null rest ||
                                                head rest /= '`' = errToken "Unterminated infix identifier" p 
                                                                 : doScan p1 rest
                                              | iskw ident       = errToken ("Keyword used as infix identifier: " ++ ident) p 
                                                                 : doScan (advc 1 p1) (tail rest)
                                              | otherwise        = valueToken TkOp ident p 
                                                                 : doScan (advc 1 p1) (tail rest)
                                   in tokens
                          | otherwise = errToken ("Unexpected character in infix identifier: " ++ show c) p 
                                      : doScan (adv p c) s
                  in res
   -}
   doScan p cs@(c:s)
     | isSymbol c = reserved [c] p
                  : doScan(advc 1 p) s
     | isIdStart c || isUpper c
         = let (name', p', s')    = scanIdent (advc 1 p) s
               name               = c:name'
               tok                = if iskw name
                                    then reserved name p
                                    else if null name' && isSymbol c
                                    then reserved [c] p
                                    else valueToken (if isIdStart c then TkVarid else TkConid) name p
           in tok :  doScan p' s'
     | isOpsym c = let (name, s') = span isOpsym cs
                       tok | isop name = reserved name p
                           | c==':'    = valueToken TkConOp name p
                           | otherwise = valueToken TkOp name p
                   in tok : doScan (foldl adv p name) s'
     | isDigit c = let (tktype,number,width,s') = getNumber cs
                   in  valueToken tktype number p : doScan (advc width p) s'
     | otherwise = errToken ("Unexpected character " ++ show c) p
                 : doScan (adv p c) s

{-

-- ks: no clean implementation of columns
readname s lc = (name,orest,nlc)
  where (line,irest) = span (/='\n') s
        orest = if null irest then "" else irest
        nlc   = if null irest then lc else (lc `advl` 1)
        name  = takename . dropWhile (\x -> not $ x `elem` "{[") $ line
        takename ln | null ln   = ""
                    | otherwise = if not (null tln) && (isAlpha . head $ tln)
                                  then if not (null rln) && (head rln `elem` "}]")
                                       then cname
                                       else err lc 1
                                  else err lc 1
          where (cname, rln) = span validChar tln
                tln          = tail ln
                validChar c  = isAlpha c || c `elem` ".-_" || isDigit c

-- ks: changed definition from (lc+1) to (lc)
err lc 1 = error ("in scanner bad name definition" ++ maybeshow (lc))
err lc fn 2
   = error ("in scanner not a valid name in file inclusion" ++ maybeshow (lc))
-}
lexNest :: (Pos -> String -> [Token]) 
        -> Pos 
        -> String 
        -> [Token]
lexNest cont pos inp = lexNest' cont pos inp
 where lexNest' c p ('-':'}':s) = c (advc 2 p) s
       lexNest' c p ('{':'-':s) = lexNest' (lexNest' c) (advc 2 p) s
       lexNest' c p (x:s)       = lexNest' c (adv p x) s
       lexNest' _ _ []          = [ errToken "Unterminated nested comment" pos]

scanString :: String -> (String,Int,String)
scanString []            = ("",0,[])
scanString ('\\':'&':xs) = let (str,w,r) = scanString xs
                           in (str,w+2,r)
scanString ('\'':xs)     = let (str,w,r) = scanString xs
                           in ('\'': str,w+1,r)
scanString xs = let (ch,cw,cr) = getchar xs
                    (str,w,r)  = scanString cr
                    str' = maybe "" (:str) ch
                in maybe ("",0,xs) (\c -> (c:str,cw+w,r)) ch

scanChar :: [Char] -> (Maybe Char,Int,[Char])
scanChar ('"' :xs) = (Just '"',1,xs)
scanChar xs        = getchar xs

getchar :: [Char] -> (Maybe Char,Int,[Char])
getchar []          = (Nothing,0,[])
getchar s@('\n':_ ) = (Nothing,0,s )
getchar s@('\t':_ ) = (Nothing,0,s)
getchar s@('\'':_ ) = (Nothing,0,s)
getchar s@('\"' :_ ) = (Nothing,0,s)
getchar   ('\\':xs) = let (c,l,r) = getEscChar xs
                      in (c,l+1,r)
getchar (x:xs)      = (Just x,1,xs)

getEscChar :: [Char] -> (Maybe Char,Int,[Char])
getEscChar [] = (Nothing,0,[])
getEscChar s@(x:xs) | isDigit x = let (tp,n,len,rest) = getNumber s
                                      val = case tp of
                                              TkInteger8  -> readn 8  n
                                              TkInteger16 -> readn 16 n
                                              TkInteger10 -> readn 10 n
                                  in  if val >= 0 && val <= 255
                                         then (Just (chr val),len, rest)
                                         else (Nothing,1,rest)
                    | otherwise = case x `lookup` cntrChars of
                                 Nothing -> (Nothing,0,s)
                                 Just c  -> (Just c,1,xs)
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\'),('\"','\"'),('\'','\'')]

readn :: Int -> [Char] -> Int
readn base n = foldl (\r x  -> value x + base * r) 0 n

getNumber :: [Char] -> (EnumValToken,[Char],Int,[Char])
getNumber cs@(c:s)
  | c /= '0'               = num10
  | null s                 = const0
  | hs == 'x' || hs == 'X' = num16
  | hs == 'o' || hs == 'O' = num8
  | otherwise              = num10
  where (hs:ts) = s
        const0 = (TkInteger10, "0",1,s)
        num10  = let (n,r) = span isDigit cs
                 in (TkInteger10,n,length n,r)
        num16   = readNum isHexaDigit  ts TkInteger16
        num8    = readNum isOctalDigit ts TkInteger8
        readNum p ts tk
          = let nrs@(n,rs) = span p ts
            in  if null n then const0
                          else (tk         , n, 2+length n,rs)

isHexaDigit :: Char -> Bool
isHexaDigit  d = isDigit d || (d >= 'A' && d <= 'F') || (d >= 'a' && d <= 'f')

isOctalDigit :: Char -> Bool
isOctalDigit d = d >= '0' && d <= '7'

value :: Char -> Int
value c | isDigit c = ord c - ord '0'
        | isUpper c = ord c - ord 'A' + 10
        | isLower c = ord c - ord 'a' + 10

%%]

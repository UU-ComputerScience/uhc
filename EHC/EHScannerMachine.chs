% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[7 module EHScannerMachine import(Char,List,Maybe,UU.Util.BinaryTrees,UU.Scanner.Token,UU.Scanner.Position)
scanFile :: [String] -> [String] -> String -> String -> [String] -> FilePath -> IO [Token]
scanFile keywordstxt keywordsops specchars opchars specpairs fn = 
        do txt <- readFile fn
           return (scan keywordstxt keywordsops specchars opchars specpairs (initPos fn) txt) 

scan :: [String] -> [String] -> String -> String -> [String] -> Pos -> String -> [Token]
scan keywordstxt keywordsops specchars opchars specpairs pos input
  = doScan pos input

 where
   locatein :: Ord a => [a] -> a -> Bool
   locatein es = isJust . btLocateIn compare (tab2tree (sort es))
   iskw     = locatein keywordstxt
   isop     = locatein keywordsops
   isSymbol = locatein specchars
   isOpsym  = locatein opchars
   isPairSym= locatein specpairs

   isIdStart c = isLower c || c == '_'

   isIdChar c =  isAlphaNum c
              || c == '\''
              || c == '_'

   scanIdent p s = let (name,rest) = span isIdChar s
                   in (name,advc (length name) p,rest)


   doScan p [] = []
   doScan p (c:s)        | isSpace c = let (sp,next) = span isSpace s
                                       in  doScan (foldl adv p (c:sp)) next

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

   doScan p cs@(c:c2:s)
     | isPairSym sym = reserved sym p : doScan(advc 2 p) s
         where sym = [c,c2]
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

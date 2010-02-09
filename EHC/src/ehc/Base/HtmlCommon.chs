Utility functions for HTML generation

%%[1 hs module {%{EH}Base.HtmlCommon}
%%]

%%[7_2 hs import({%{EH}Base.Common}, EH.Util.ParseUtils,  {%{EH}Ty}, {%{EH}EH}, qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set), EH.Util.Pretty, Data.List(sortBy))
%%]

%%[7_2 hs

ppEnclosingTag :: (PP a, PP b, PP c) => a -> b -> c -> PP_Doc
ppEnclosingTag begin middle end
  = invisible (pp begin) >|< pp middle >|< invisible (pp end)

ppKeywordHTML :: PP a => a -> PP_Doc
ppKeywordHTML p
  = ppEnclosingTag "<span class=\"keyword\">" p "</span>"

ppKeywordLink :: PP a => UID -> a -> PP_Doc
ppKeywordLink u p
  = ppEnclosingTag ("<a href=\"#\" class=\"keywlink\" onclick=\"showInfoBlock('" >|< show u >|< "'); return false;\">") p "</a>"

ppIdentHTML :: PP a => a -> PP_Doc
ppIdentHTML p
  = ppEnclosingTag "<span class=\"ident\">" p "</span>"

ppPatIdentHTML :: PP a => a -> PP_Doc
ppPatIdentHTML p
  = ppEnclosingTag "<span class=\"patid\">" p "</span>"

ppSymbolHTML :: PP a => a -> PP_Doc
ppSymbolHTML p
  = ppEnclosingTag "<span class=\"symbol\">" p "</span>"

ppParensHTML :: UID -> PP_Doc -> PP_Doc
ppParensHTML uid p
  = let (uid1, uid2) = mkNewUID uid
        mkSpan s u = ppEnclosingTag ("<a" >#< "id=\"u" >|< show u >|< "\" href=\"#\" class=\"parens\" onclick=\"return false;\" onmouseover=\"highlightOn('"++show uid1++"','"++show uid2++"');\" onmouseout=\"highlightOff('"++show uid1++"','"++show uid2++"');\">") s "</a>"
     in mkSpan "(" uid1 >|< p >|< mkSpan ")" uid2

ppIdentLink :: PP a => UID -> a -> PP_Doc
ppIdentLink u p
  = ppEnclosingTag ("<a href=\"#\" class=\"infolink\" onclick=\"showInfoBlock('" >|< show u >|< "'); return false;\">") p "</a>"

ppSymbolLink :: PP a => UID -> a -> PP_Doc
ppSymbolLink u p
  = ppEnclosingTag ("<a href=\"#\" class=\"symbollink\" onclick=\"showInfoBlock('" >|< show u >|< "'); return false;\">") p "</a>"

ppConHTML :: (Annotation Ty -> PP_Doc) -> HsName -> Annotation Ty -> Annotations Ty -> PP_Doc
ppConHTML annPPF nm ann anns
  = pp (show nm) >|< ppEnclosingTag "<sup>" (annPPF ann) "</sup>" >|< ppEnclosingTag "<sub>" (ppListSep "{" "}" "," [ annPPF a | a <- Set.toList anns ]) "</sub>"

ppVarHTML :: String -> PP_Doc -> PP_Doc
ppVarHTML s p
  = ppIdentHTML s >|< ppEnclosingTag "<sup>" p "</sup>"

ppQuantHTML :: String -> Bool -> PP_Doc -> PP_Doc
ppQuantHTML s isForall p
  = (if isForall then ppSymbolHTML ("&" >|< invisible (pp "forall;")) else ppSymbolHTML ("&" >|< invisible (pp "exist;"))) >|< s >|< ppSymbolHTML "." >#< p

ppArrowHTML :: PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
ppArrowHTML ann f a
  = f >#< ("&" >|< invisible (pp "rarr;")) >|< ppEnclosingTag "<sup>" ann "</sup>" >#< a

ppExtHTML :: [(HsName, PP_Doc)] -> PP_Doc
ppExtHTML xs
  = ppListSep "(" ")" "," [ ppSymbolHTML (pp nm >|< "=") >|< p | (nm, p) <- xs' ]
  where
    xs' = sortBy (\a b -> fst a `cmpHsNameOnNm` fst b) xs

ppAnn :: (Annotation Ty -> String) -> Annotation Ty -> PP_Doc
ppAnn f ann
  = ppEnclosingTag ("<a href=\"#\" onclick=\"return false;\" class=\"ann\" title=\"" >|< ppA >|< "\">") (f ann) "</a>"
  where
    ppA = mk ann
    mk a
      = let uid   = annUID a
            mInst = annInstFrom a
            mRef  = annOnRefTp a
         in f ann >#< "[" >|< show uid >|< "," >#< mkM mRef >|< "]" >#< mkM mInst
      
    mkM (Just a) = mk a
    mkM Nothing  = pp "_"

mkInfoHtmlBlock :: PP a => UID -> a -> PP_Doc
mkInfoHtmlBlock u p
  = ppEnclosingTag ("<span id=\"u" >|< show u >|< "\" class=\"info\">") p "</span>"

convertSpaces :: String -> String
convertSpaces
  = reverse . mk True ""
  where
    mk True aux (' ':xs)
      = mk True (";psbn&" ++ aux) xs
    mk isOutsideTag aux (x:xs)
      | x == '<'  = mk False (x:aux) xs
      | x == '>'  = mk True (x:aux) xs
      | x == '\r' = case xs of
                      ('\n':ys) -> mk isOutsideTag (">rb<\n\r" ++ aux) ys
                      ys        -> mk isOutsideTag (">rb<\r" ++ aux) ys
      | x == '\n' = case xs of
                      ('\r':ys) -> mk isOutsideTag (">rb<\n\r" ++ aux) ys
                      ys        -> mk isOutsideTag (">rb<\n" ++ aux) ys
      | otherwise = mk isOutsideTag (x:aux) xs
    mk _ aux []
      = aux

mkHTMLDocument :: PP_Doc -> String
mkHTMLDocument p
  = disp enlarged 9999 ""
  where
    enlarged :: PP_Doc
    enlarged
      =   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
      >-< "<html lang=\"en\">"
      >-< "<head>"
      >-< "<title>Uniqueness output inspection</title>"
      >-< "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"
      >-< "<style type=\"text/css\">"
      >-< "body { font-family: verdana"
      >-< "     ; font-size: 12px"
      >-< "     ; color: black"
      >-< "     ; background: white"
      >-< "     }"
      >-< ".keyword  { color: black; font-weight: bold }"
      >-< ".keywlink { color: black; font-weight: bold; text-decoration: none }"
      >-< ".symbol   { color: gray }"
      >-< ".symbollink { color: gray; text-decoration: none }"
      >-< ".ident    { color: green; text-decoration: none }"
      >-< ".patid    { color: darkgreen; text-decoration: none }"
      >-< ".parens   { color: gray; text-decoration: none }"
      >-< ".info     { display: none }"
      >-< ".infolink { color: green; text-decoration: none }"
      >-< ".ann      { color: darkred; text-decoration: none }"
      >-< ".bndg     { color: lightgray }"
      >-< ".tt       { font-family: Courier }"
      >-< "A:hover { color: darkblue } "
      >-< "</style>"
      >-< "<script type=\"text/javascript\">"
      >-< "<!--"
      >-< "function highlightOn(uA, uB)"
      >-< "{"
      >-< "  var eA = document.getElementById('u'+uA);"
      >-< "  var eB = document.getElementById('u'+uB);"
      >-< "  eA.style.color=\"red\";"
      >-< "  eB.style.color=\"red\";"
      >-< "}"
      >-< "function highlightOff(uA, uB)"
      >-< "{"
      >-< "  var eA = document.getElementById('u'+uA);"
      >-< "  var eB = document.getElementById('u'+uB);"
      >-< "  eA.style.color=\"gray\";"
      >-< "  eB.style.color=\"gray\";"
      >-< "}"
      >-< "var lastvisible=false;"
      >-< "function showInfoBlock(u)"
      >-< "{"
      >-< "  if (lastvisible)"
      >-< "  {"
      >-< "    var e2 = document.getElementById('u'+lastvisible);"
      >-< "    e2.style.display=\"none\";"
      >-< "    lastvisible = false;"
      >-< "  }"
      >-< "  var e = document.getElementById('u'+u);"
      >-< "  e.style.display=\"inline\";"
      >-< "  lastvisible = u;"
      >-< "}"
      >-< "//-->"
      >-< "</script>"
      >-< "</head>"
      >-< "<body>"
      >-< "<p class=\"tt\">"
      >-< convertSpaces (disp p 9999 "")
      >-< "</p>"
      >-< "</body>"
      >-< "</html>"

%%]

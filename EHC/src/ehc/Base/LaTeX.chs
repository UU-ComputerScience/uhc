%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LaTeX + lhs2TeX generation utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.LaTeX} import(EH.Util.Utils,EH.Util.Pretty, Data.List)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(ltxCall,ltxCallList,ltxKeyword,ltxPackage,ltxEnvironment,ltxDocument,ltxLandscape,ltxMathmode,"(>##<)")
ltxCall               :: PP x => String -> x -> PP_Doc
ltxCall str doc       = pp ("\\" ++ str) >|< ppCurlys doc

ltxCallList           :: PP x => String -> [x] -> PP_Doc
ltxCallList str docs  = foldl (\a b -> a >-< ppCurlys b) (pp ("\\"++str)) docs

ltxKeyword            :: PP x => x -> PP_Doc
ltxKeyword str        = ltxCall "textbf" str

ltxPackage            :: PP x => x -> PP_Doc
ltxPackage pkg        = ltxCall "usepackage" pkg

ltxEnvironment          :: PP x => String -> x -> PP_Doc
ltxEnvironment env doc  = ltxCall "begin" (pp env) >-< doc >-< ltxCall "end" (pp env)

ltxDocument, ltxLandscape, ltxMathmode
              :: PP x => x -> PP_Doc
ltxDocument   = ltxEnvironment "document"
ltxLandscape  = ltxEnvironment "landscape"
ltxMathmode   = ltxEnvironment "displaymath"

ltxQuad       :: PP_Doc
ltxQuad       = pp "\\quad"

ltxUnknown    :: PP_Doc
ltxUnknown    = pp "\\Unknown"

(>##<) :: (PP x, PP y) => x -> y -> PP_Doc
a >##< b = a >|< pp "~" >|< b

%%]

%%[1 export(l2tText,l2tPackage)
l2tText :: PP x => x -> PP_Doc
l2tText t = "|" >#< t >#< "|"

l2tPackage            :: PP x => x -> PP_Doc
l2tPackage pkg        = "%include" >#< pkg >|< ".fmt"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Document for derivation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(l2tDtHeader)
l2tDtHeader  :: [String] -> [String] -> PP_Doc
l2tDtHeader l2tPkgs ltxPkgs
  =   ltxCall "documentclass" (pp "article")
  >-< (vlist $ map (l2tPackage . pp) l2tPkgs)
  >-< (vlist $ map (ltxPackage . pp) ltxPkgs)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derivation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(ltxDtOver)
ltxDtOver :: String -> [PP_Doc] -> String -> PP_Doc -> PP_Doc
ltxDtOver format premises name conclusion
  = pp ("\\ovr[" ++ format ++ "]") 
    >-< ppCurlys premises' 
    >-< ppBrackets (pp name)
    >-< ppCurlys conclusion
  where premises' =  if null premises 
                     then empty 
                     else foldr1 (\a b -> a >-< ltxQuad >-< b) premises
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LaTeX + lhs2TeX generation utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.LaTeX} import(EH.Util.Utils,EH.Util.Pretty, Data.List)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(ltxNl,ltxCall0,ltxCall,ltxCallArgs,ltxCallOptsArgs,ltxKeyword,ltxPackage,ltxEnvironment,ltxEnvironmentArgs,ltxDocument,ltxLandscape,ltxMath,ltxDisplayMath,"(>##<)")
ltxCallOptsArgs :: PP x => String -> [x] -> [x] -> PP_Doc
ltxCallOptsArgs str opts args
  = c >|< o opts >|< a
  where o [] = empty
        o l  = ppBracketsCommas l
        a    = map ppCurlys args
        c    = pp ("\\"++str)

ltxCallArgs           :: PP x => String -> [x] -> PP_Doc
ltxCallArgs str args  = ltxCallOptsArgs str [] args

ltxCall               :: PP x => String -> x -> PP_Doc
ltxCall str arg       = ltxCallArgs str [arg]

ltxCall0              :: String -> PP_Doc
ltxCall0 str          = ltxCallArgs str ([] :: [PP_Doc])

ltxKeyword            :: PP x => x -> PP_Doc
ltxKeyword str        = ltxCall "textbf" str

ltxPackage            :: PP x => x -> [x] -> PP_Doc
ltxPackage pkg opts   = ltxCallOptsArgs "usepackage" opts [pkg]

ltxEnvironmentArgs          :: (PP x) => String -> [x] -> x -> PP_Doc
ltxEnvironmentArgs env args doc  = ltxCallArgs "begin" (pp env : map pp args) >-< doc >-< ltxCall "end" (pp env)

ltxEnvironment          :: PP x => String -> x -> PP_Doc
ltxEnvironment env doc  = ltxEnvironmentArgs env [] doc

ltxDocument, ltxLandscape, ltxDisplayMath, ltxMath
              :: PP x => x -> PP_Doc
ltxDocument     = ltxEnvironment "document"
ltxLandscape    = ltxEnvironment "landscape"
ltxMath         = ltxEnvironment "math"
ltxDisplayMath  = ltxEnvironment "displaymath"

ltxQuad       :: PP_Doc
ltxQuad       = ltxCall0 "quad"

ltxNl       :: PP_Doc
ltxNl       = pp "\\\\"

ltxUnknown    :: PP_Doc
ltxUnknown    = pp "\\Unknown"

(>##<) :: (PP x, PP y) => x -> y -> PP_Doc
a >##< b = a >|< pp "~" >|< b

%%]

%%[99 export(l2tFormat,l2tText,l2tPackage)
l2tFormat :: PP x => x -> Maybe x -> PP_Doc
l2tFormat i mv = "%format" >#< i >#< maybe empty (\v -> "=" >#< v) mv

l2tText :: PP x => x -> PP_Doc
l2tText t = "|" >#< t >#< "|"

l2tPackage            :: PP x => x -> PP_Doc
l2tPackage pkg        = "%include" >#< pkg >|< ".fmt"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Document for derivation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(l2tDtHeader)
l2tDtHeader  :: [String] -> [(String,[PP_Doc])] -> PP_Doc
l2tDtHeader l2tPkgs ltxPkgs
  =   ltxCall "documentclass" (pp "article")
  >-< (vlist $ map (l2tPackage . pp) l2tPkgs)
  >-< (vlist $ map (\(p,o) -> ltxPackage (pp p) o) ltxPkgs)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derivation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(ltxDtOver)
ltxDtOver :: String -> Bool -> [PP_Doc] -> String -> PP_Doc -> PP_Doc
ltxDtOver format isTop premises name conclusion
  = pp ("\\" ++ ovr ++ "[" ++ format ++ "]") 
    >-< ppCurlys premises' 
    >-< ppBrackets (pp name)
    >-< ppCurlys conclusion
  where premises' =  if null premises 
                     then empty 
                     else foldr1 (\a b -> a >-< ltxQuad >-< b) premises
        ovr = if isTop then "Ovr" else "ovr"
%%]


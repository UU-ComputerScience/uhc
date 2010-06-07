{-% -----------------------------------------------------------------------------
% $Id: HsPretty.hs,v 1.1 2009/07/18 18:33:52 fdomin Exp $
%
%  Operaciones para imprimir los programas parseados.
%
% -----------------------------------------------------------------------------}

%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.HsPretty}
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
--import Text.PrettyPrint
--import HsPrec
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
import Control.Monad(mplus,msum)
import List((\\),transpose)
import Debug.Trace(trace)
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})
%%]

instance Show Prog where   
    show (Prog defs) = render.vcat.mapDocSeparator (text ""$$) $ defs
    {- o : show (Prog defs) = fullRender PageMode 100 1.5 string_txt "" (showDoc defs)
     string_txt (Chr c)   s  = c:s
     string_txt (Str s1)  s2 = s1 ++ s2
     string_txt (PStr s1) s2 = s1 ++ s2
    -}

instance Show Def where
  show = render . showDoc

instance Show Term where   
    show t = render.showDoc$ t

-- Esta clase agrupa los tipos de los cuales es posible construir
-- un documento de la librería Pretty.
class ShowDoc a where
    showDoc::a->Doc
    showDocPrec:: Precedence -> LeftParam -> a -> Doc

    showDoc = showDocPrec (0,None) False
    showDocPrec _ _ = showDoc -- Usa dos parámetros uno para el valor de
                      -- precedencia, y la asociatividad, estos
                  -- permiten determinar la necesidad de paréntesis.


-- =================================================
-- FUNCIONES AUXILIARES 
-- =================================================


-- Coloca un separador entre la representación de strings de elementos de
-- una lista.
-- miShowList ";" [1,2,3,4] -> "1;2;3;4"
mishowList:: Show a => String->[a]->String
mishowList _ [] = ""
mishowList separador elems = foldr1 ((++).(++separador)) (map show elems)

-- ShowDoc f [1,2,3,4] -> [showDoc 1,...,f(showDoc 3),f(showDoc 4)]
mapDocSeparator:: ShowDoc a => (Doc->Doc)->[a]->[Doc]
mapDocSeparator separador = mapSeparator separador . map showDoc

-- Muestra una tupla de elementos.
showTuple :: ShowDoc a => [a]->Doc
showTuple [a] = showDoc a
showTuple ls = char '('<> (hcat$ mapDocSeparator (char ','<>) ls)<>char ')'

-- Aplica una función a todos los argumentos de una lista menos al
-- primero. Se utiliza para insertar separadores en la lista.
mapSeparator::(a->a)->[a]->[a]
mapSeparator _  [] = []
mapSeparator separador (l:ls) = l:map separador ls

-- Inserta paréntesis o no, de acuerdo al valor del primer parámetro,
-- utilizando <> delante y ++ detrás de la expresión en el segundo 
-- parámetro.
parlist:: Bool -> [Doc] -> Doc
parlist paren content = if paren 
                          then  char '(' <> 
                                cat 
                                 (mapSeparator (text ""<+>) content
                                 ++[char ')'])
                          else sep content

tab,stab::Int
tab=4      -- Indentación grande
stab=2     -- Indentación pequeña

-- Precedencias (ver HsPrec)
ttupleprec,tlambprec,tletprec,tcaseprec,
 tfappprec,thyloprec,tcappprec,tappprec,tsumprec::Precedence
ttupleprec=(0,None)
tlambprec=(0,RightAsoc)
tletprec=(0,RightAsoc)
tcaseprec=(0,RightAsoc)
thyloprec=(0,RightAsoc)
tsumprec =(1,None)
tfappprec=(maxprec,LeftAsoc)
tcappprec=(maxprec,LeftAsoc)
tappprec=(maxprec,LeftAsoc)

dec (p,a) = (p-1,a)

maxprec::Int
maxprec=10

-- ================================================================
-- Funciones de conversión.
-- ================================================================


-- Pretty print para tuplas de 2 elementos.

instance (ShowDoc a,ShowDoc b)  => ShowDoc (a,b) where
    showDoc (a,b) = char '('<>showDoc a<>char ','<+> showDoc b <>char ')'

instance ShowDoc Bool where
    showDoc True = text "True"
    showDoc False = text "False"

instance ShowDoc Def where
    showDoc (Defvalue name t) = vcat$ map showDef$ splitCase vs t'
     where getParams (Tlamb bv t) = let (l,t') = getParams t in (bv:l,t')
           getParams t = ([],t)
           isVar (Tvar _) = True
           isVar _ = False
           (vs,t') = getParams t
           -- tries to separate a case definition into equations
           -- it returns a list containing one element for each derived equation
           -- [(equation patterns,equation term)]
           splitCase :: [Boundvar] -> Term -> [([Pattern],Term)]
           splitCase bvs t = maybe [(map bv2pat bvs,t)] (uncurry$ zipWith (\p t->(patList p,t)))$ 
                                                         mergeCasePatterns' p t
             where p = ptuple$ map bv2pat bvs
           patList (Ptuple ps) = ps
           patList p = [p]

           showDef (ps,t) = sep [text (show name) <+> hsep (map (toDoc (vars t)) ps) <+> char '=' 
                                , nest 8$ showDoc t]
           toDoc vs p = showDocPrec tcappprec False (removeSpuriousPas vs p)

    showDoc (Defdata dat) = showDoc dat

-- Flattens nested case terms by carefully replacing patterns into patterns to remove
-- inner case constructs and increase the amount of alternatives in the outer case
-- constructs.

%%[(8 codegen)

mergeCasePatterns :: Term -> Term
mergeCasePatterns t = mergeCPat id t
  where mergeCPat bvs (Tlamb bv t) = Tlamb bv$ mergeCPat (bvs . (bv:)) t
        mergeCPat bvs t = 
           let ps = map bv2pat (bvs [])
               pvs = lastVarsList ps
            in if null ps 
                 then case t of
                       Tcase t0 ps ts -> (\(ps,ts)-> Tcase t0 (concat ps) (concat ts))$
                                                     unzip$ map unzip$ zipWith ((map removePas.) . mergeCase) ps ts 
                       _ -> t 
                 else if isTcase t then
                        delCases$ uncurry (Tcase (ttuple (map Tvar pvs)))$ unzip$ map removePas$ mergeCase (ptuple$ map Pvar pvs) t
					  else t
        mergeCase :: Pattern -> Term -> [(Pattern,Term)]
        mergeCase p t = case mergeCasePatterns' p t of
                         Just (ps,ts) -> concat$ zipWith mergeCase ps ts
                         Nothing -> [(p,t)]
        removePas (p,t) = (removeSpuriousPas (vars t) p,t)
        isTcase (Tcase _ _ _) = True
        isTcase _ = False

-- Removes Pas patterns which are not in a given set of variables.

removeSpuriousPas ::  [Variable] -> Pattern -> Pattern
removeSpuriousPas vs (Pas v p@(Pas v' p')) | v==v' = removeSpuriousPas vs p
removeSpuriousPas vs (Pas v p@(Pvar v')) |  v==v' = p
                                         |  p==pany = if elem v vs then Pvar v else pany
removeSpuriousPas vs (Pas v p) | elem v vs = Pas v (removeSpuriousPas vs p)
                               | otherwise = removeSpuriousPas vs p
removeSpuriousPas vs p@(Pvar v) = p
removeSpuriousPas vs p@(Plit _) = p
removeSpuriousPas vs (Ptuple ps) = Ptuple (map (removeSpuriousPas vs) ps)
removeSpuriousPas vs (Pcons c ps) = Pcons c (map (removeSpuriousPas vs) ps)

-- Merges the cases in the term to the given pattern. Returns Nothing
-- if there are no cases to merge.

mergeCasePatterns' :: Pattern -> Term -> Maybe ([Pattern],[Term])
mergeCasePatterns' p t = case removeAllCaseVar Nothing t of
                           Just res -> mergeCasePatterns'' p res `mplus` Just ([p],[res])
                           _ -> mergeCasePatterns'' p t
  where removeAllCaseVar res t = maybe res (\mt->removeAllCaseVar (Just mt) mt)$ removeCaseVars t
        removeCaseVars (Tcase t0 ps ts) 
                       | length t0s > 1, 
                         Just i<-msum (zipWith3 findVarPattern [0..] t0s pss) = Just (Tcase (listToTerm$ del i t0s) (map (removePattern i) ps) ts)
            where t0s = termList t0
                  pss = transpose$ map patList ps
        removeCaseVars _ = Nothing
        findVarPattern i (Tvar v) ps | all (isPatternVar v) ps = Just i
        findVarPattern i _ ps = Nothing
        isPatternVar v (Pvar v') = v==v'
        isPatternVar v _ = False
        removePattern i (Ptuple ps) = toPat$ del i ps
        removePattern i _ = error "mergeCasePatterns': something that shouldn't had happened happened."
        listToTerm [t] = t
        listToTerm ts = Ttuple False ts
        termList (Ttuple _ ts) = ts
        termList t = [t]
        patList (Ptuple ps) = ps
        patList p = [p]
        toPat [p] = p
        toPat ps = Ptuple ps
        del i ls = take i ls ++ drop (i+1) ls

mergeCasePatterns'' :: Pattern -> Term -> Maybe ([Pattern],[Term])
mergeCasePatterns'' p (Tcase t0 ps ts)
            | all isAllowedTerm t0s 
              && orderMatches lvp t0s
              && all ((==length t0s) . length) pss
              && any (not.null.fst) sustss =
         Just (map (flip substitutePattern p . fst)$ sustss ,map snd sustss)
  where t0s = termList t0
        tvs = vars t0s
        pss = map patList ps
        sustss = zipWith (makeSustPairs tvs) pss ts
        lvp = lastVars p
        isAllowedTerm (Tvar v) = elem v lvp
        isAllowedTerm _ = False
        makeSustPairs tvs ps t = let ss = zipWith (makeSustPair t (vars t)) tvs ps
                                  in (map fst ss,substitution (concat . map snd$ ss) t)
        makeSustPair t tsvs v (Pvar v') | notElem v (varsB t) = ((v,Pvar v),[(v',Tvar v)])
        makeSustPair t tsvs v p | p/=pany = ((v,Pas v p),[])
                                | elem v tsvs = ((v,Pvar v),[])
		                        | otherwise = ((v,pany),[])
        patList (Ptuple ps) = ps
        patList p = [p]
        termList (Ttuple _ ts) = ts
        termList t = [t]
        orderMatches (v:lvp) t0s@(Tvar v':t0ss) | v==v' = orderMatches lvp t0ss
                                                | otherwise = orderMatches lvp t0s
        orderMatches _ [] = True
        orderMatches _ _ = False
mergeCasePatterns'' _ t = Nothing

substitutePattern :: [(Variable,Pattern)] -> Pattern -> Pattern
substitutePattern subst p@(Pvar v) = maybe p id $ lookup v subst 
substitutePattern subst p@(Plit _) = p 
substitutePattern subst (Ptuple ps) = Ptuple$ map (substitutePattern subst) ps 
substitutePattern subst (Pcons c ps) = Pcons c$ map (substitutePattern subst) ps 
substitutePattern subst (Pas v p) = Pas v$ substitutePattern subst p 


-- lastVars returns the variables that can be replaced without breaking the pattern matching sequence.
-- e.g. in (C a (C2 b c) d) the variables that can be safely replaced are b c and d.

lastVars :: Pattern -> [Variable]
lastVars (Pvar v) = [v]
lastVars (Pcons c ps) = lastVarsList ps
lastVars (Ptuple ps) = lastVarsList ps
lastVars (Plit _) = []
lastVars (Pas _ p) = lastVars p
lastVarsList ps = let (vps,others) = break (not.isPvar) (reverse ps)
                   in if null others then reverse (vars vps)
                        else lastVars (head others)++reverse (vars vps)
  where isPvar (Pvar v) = True
        isPvar _ = False


%%]

instance Show Tipo where
    show (Typecons name []) = name  
    show (Typecons name tipos) = name ++ foldr (\t1 r -> " "++ showsPrec 1 t1 r) "" tipos
    show (Typefunc tipos) = "("++ show tipos ++ ")"
    show (Typetuple tipos) = showChar '(' (mishowList "," tipos) ++ ")"
    show (Typelist tipo) = showChar '[' (show tipo) ++ "]" 
    show (Typevar name) = show name 
    showList [] next = next
    showList tipos next = mishowList " -> " tipos ++ next
    showsPrec p t@(Typecons _ []) s = show t ++ s
    showsPrec p t@(Typecons _ _) s = if p>0 then "(" ++ show t ++ ")" ++ s else show t ++ s
    showsPrec p t@(Typefunc _) s = if p>0 then "(" ++ show t ++ ")" ++ s else show t ++ s
    showsPrec _ t s = show t ++ s

instance ShowDoc Data where
    showDoc (Data (name,vars) lcs)=
               sep [text ("data "++ name 
                          ++ foldr ((++).(" "++).show) [] vars ++ if null lcs then "" else " ="),
                    nest tab (showDocConsList lcs)
                   ]
           where showDocConsList ((hc,ha):lcs) = 
                                sep $ (text hc<+>showDocList ha):
                                      [text ("| "++cons)<+>showDocList args
                                       | (cons,args)<-lcs]
                 showDocConsList [] = empty
                 showDocList args = hsep (map showDoc args)

instance ShowDoc ArgumentDeclaration where
    showDoc (Avar v) = text (show v)
    showDoc (Acons name []) = text name
    showDoc (Acons name vars) = 
           text ("("++name ++ foldr ((++).(" "++).show) [] vars++")")

instance ShowDoc Variable where
    showDoc v = text (show v)

instance ShowDoc Term where
    showDocPrec _ _ (Tvar name) = text (show name)
    showDocPrec _ _ (Tlit literal) = text (show literal)

    showDocPrec _ _ (Ttuple _ [Tvar name]) = text (show name)
    showDocPrec _ _ (Ttuple _ terms) =  
           let
              tdocs = map (showDocPrec ttupleprec False) terms
           in          
               cat $ [char '(']
                     ++ map (nest 1) (mapSeparator (char ','<>) tdocs)
                     ++ [char ')']

    -- La siguiente operación está comentada para ilustrar el 
    -- comportamiento general de cada definición.
    showDocPrec prec left (Tlamb boundvar term) =
                let
        -- Obtengo la representación del subtérmino.
                   t=showDocPrec tlambprec False term
        -- Construyo la representación de la lambda espresión.
                   content = [char '\\' <> text (show boundvar) <+> text "->",
                              nest tab t]
                -- Pregunto si hay que parentizar (función de HsPrec).
                -- Para hacer la pregunta se especifica la precedencia y
                -- asociatividad del operador que contiene la lambda
                -- expresión, y la precedencia y asociatividad de la 
                -- lambda expresión.
                   paren = parentizar prec left tlambprec
                -- Parentizo la representación si es necesario.
                in parlist paren content

    showDocPrec prec left (Tlet variable term1 term2) =
                let
                   t1=showDocPrec tletprec False term1
                   t2=showDocPrec tletprec False term2
                   content = [(text "let" <+> text (show variable) <+> equals <+> t1)
                              $$ nest 1 (text "in" <+> t2)]
                   paren = parentizar prec left tletprec
                in parlist paren content

    showDocPrec prec left (Tif term0 term1 term2) =
                let
                   t0=showDocPrec tletprec False term0
                   t1=showDocPrec tletprec False term1
                   t2=showDocPrec tletprec False term2
                   content = [vcat [sep [text "if" <+> t0,nest stab (text "then"<+>t1)],
                                    nest stab (text "else" <+> t2)]]
                   paren = parentizar prec left tletprec
                in parlist paren content

    showDocPrec prec left (Tpar t) = char '(' <+> showDocPrec (0,None) False t <+> char ')'

    showDocPrec prec left (Tcase term ps' ts') =
                let
                   tr=showDocPrec tcaseprec False term
                   ts=map (showDocPrec tcaseprec False) ts'
                   ps=map (showDocPrec tcaseprec False) ps'
                   content = [text "case" <+> tr <+> text "of",
                              nest stab $
                                 vcat [sep [p <+> text "->", nest tab t]
                                      | (p,t)<-zip ps ts]
                             ]
                   paren = parentizar prec left tcaseprec
                in parlist paren content

    showDocPrec _ _ (Tfapp variable [] ) = text (show variable)
    showDocPrec prec left (Tfapp variable terms) =
                let
                   tds@(td:tdocs)=map (flip (showDocPrec ((if infx then dec else id)$ op_precedence))) terms
                   content 
                        | infx && length terms>1 = 
                                 (td True <+> text (show variable)):
                                 map (\t->nest 2$ t False) tdocs
                        | infx = [td True<>text (show variable)]
                        | otherwise = text (show variable):
                                      map (\t->nest 2$ t False) tds
                   paren = infx && length terms<=1 || parentizar prec left op_precedence
                in
                   parlist paren content
              where infx = esinfijo variable
                    esinfijo (Vgen _ _) = False
                    esinfijo (Vuserdef ('@':'b':cs)) = False
                    esinfijo (Vuserdef (c:cs)) = not ((('a'<=c)&&(c<='z'))||(c=='_'))
                    esinfijo _ = error infix_Operator_Without_Characters_In_Name
                    op_precedence | infx && variable/=Vuserdef "/" && variable/=Vuserdef "-"
                                         && variable/=Vuserdef "+"
                                        = (1,RightAsoc)
                                  | otherwise = tfappprec


    showDocPrec _ _ (Tcapp constructor []) = text constructor
    showDocPrec prec left (Tcapp constructor terms) =
                let
                   tds@(td:tdocs)=map (flip (showDocPrec ((if esinfijo then dec else id)$ op_precedence))) terms
                   content 
                    | esinfijo && length terms>1 = td True <+> text constructor <+>
                                                   hsep (map (\t->t False) tdocs)
                    | esinfijo = td True<>text constructor
                    | otherwise = text constructor <+> 
                                  hsep (map (\t->t False) tds)
                   paren = esinfijo && length terms<=1 || parentizar prec left op_precedence
                in
                   hpar paren content
              where esinfijo = case constructor of 
                                "_" -> False
                                (c:_)-> not (('A'<=c)&&(c<='Z')) && constructor/="[]"
                                _ -> error infix_Constructor_Without_Characters_In_Name
                    op_precedence | esinfijo = (fst tcappprec,RightAsoc)
                                  | otherwise = tcappprec

    showDocPrec prec left (Tapp term1 term2) =
               let
                  t1=showDocPrec tappprec True term1
                  t2=showDocPrec tappprec False term2
                  paren = parentizar prec left tappprec
                  content = t1 <+> t2
               in
                  hpar paren content
    showDocPrec _ _ Tbottom = text "undef"

    showDocPrec prec left (Thyloapp v i ts pos t) = showDocPrec prec left (Tfapp v (thyloArgs i ts pos t))

--    showDocPrec _ _ _ = error display_Not_Define_For_Term

instance Show Boundvar where
    show = render.showDoc

instance ShowDoc Boundvar where
    showDoc (Bvar name) = text$ show name
    showDoc (Bvtuple _ vars) = showTuple vars

instance Show Pattern where
    show (Pvar name) = show name
    show (Ptuple patterns) = "("++ mishowList "," patterns ++")"
    show (Pcons name patterns) = "("++ name ++ foldr ((++).(" "++).show) "" patterns ++")"
    show (Plit literal) = show literal
    show (Pas v p) = show v++'@':show p

instance ShowDoc Pattern where
  showDocPrec prec left p@(Pvar v) = text (show v) 
  showDocPrec prec left (Pas v p) = text (show v)<>char '@'<>showDocPrec tcappprec False p
  showDocPrec prec left (Ptuple patterns) =
               let
                  ps=map (showDocPrec ttupleprec False) patterns
               in
                  (char '('<>hcat (mapSeparator (char ','<>) ps)<>char ')')
  showDocPrec _ _ (Pcons name []) = text name
  showDocPrec prec left (Pcons name patterns) =
                let
                   tds@(td:tdocs)= map (flip (showDocPrec op_precedence)) patterns
                   content
                        | esinfijo name = td True <> text name <>
                                              hsep (map (\t->t False) tdocs)
                        | otherwise = text name <+>
                                      hsep (map (\t->t False) tds)
                   paren = parentizar prec left op_precedence
                in
                   hpar paren content
              where esinfijo (c:cs) = not (('A'<=c)&&(c<='Z'))
                    esinfijo _ = error infix_Constructor_Without_Characters_In_Name
                    op_precedence | esinfijo name = (fst tcappprec,RightAsoc)
                                  | otherwise = tcappprec
  showDocPrec _ _ (Plit literal) = text (show literal)


instance Show Literal where
    show (Lstring string) = show string
    show (Lint string) = string
    show (Lchar char) = show char
    show (Lrat string) = string

%%]
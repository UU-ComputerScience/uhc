%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.HyloFace}
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
%%]
--%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsPretty})
--%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloContext})
%%]
%%[(8 codegen)
import List(find)
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})
%%]
%%[(8 codegen)
import Control.Monad.Error
import Control.Monad.State
--import Language.Haskell.Syntax(SrcLoc(..))










class CHylo hylo where























  buildHylo :: [Variable] -> [Term] -> FusionState [hylo Phii Psi]



  getAlgebra :: hylo a ca -> Algebra a
  setAlgebra :: Algebra a -> hylo b ca -> hylo a ca



  getEta :: hylo a ca -> Eta
  setEta :: Eta -> hylo a ca -> hylo a ca



  getCoalgebra :: hylo a ca -> Coalgebra ca
  setCoalgebra :: Coalgebra ca -> hylo a cb -> hylo a ca



  getContext :: hylo a ca -> Context
  setContext :: Context -> hylo a ca -> hylo a ca



  getName :: hylo a ca -> Variable
  setName :: Variable -> hylo a ca -> hylo a ca



  getFunctor :: hylo a ca -> HyloFunctor
  setFunctor :: HyloFunctor -> hylo a ca -> hylo a ca





  consHylo :: Algebra a -> [Etai] -> HyloFunctor -> Coalgebra ca -> hylo a ca


type HyloFunctor = [HFunctor]







type Algebra a = [Acomponent a]
data Acomponent a = Acomp ([Boundvar],TermWrapper a)









newtype InF = InF (Constructor,[Term])






type Tau' a = TermWrapper (TauTerm (Acomponent a))
data Tau = Tauphi (Tau' Phii)
         | TauinF (Tau' InF)
         | Tautau (Tau' Tau)

class WrapTau a where
 wrapTau :: TermWrapper (TauTerm (Acomponent a)) -> Tau
instance WrapTau Term where
 wrapTau = Tauphi
instance WrapTau InF where
 wrapTau = TauinF
instance WrapTau Tau where
 wrapTau = Tautau

foldTau ::  (Tau' Phii->b)->
            (Tau' InF->b)->
            (Tau' Tau->b)->Tau->b
foldTau f1 f2 f3 (Tauphi t) = f1 t
foldTau f1 f2 f3 (TauinF t) = f2 t
foldTau f1 f2 f3 (Tautau t) = f3 t
mapTau :: (Tau' Phii->Tau' Phii)->(Tau' InF->Tau' InF)->(Tau' Tau->Tau' Tau)->Tau->Tau
mapTau f1 f2 f3 = foldTau (Tauphi .f1) (TauinF .f2) (Tautau .f3)

data TauTerm a = Taucons Constructor [TauTerm a] a Etai



               | Tausimple Term



               | Taupair Term (TauTerm a)



               | Taucata (Term->Term) (TauTerm a)






data TermWrapper a = TWcase Term [Pattern] [TermWrapper a]



                   | TWeta (TermWrapper a) Etai



                   | TWsimple a



                   | TWacomp (Acomponent a)



                   | TWbottom




foldTW::(Term->[Pattern]->[b]->b)->(b->Etai->b)->(a->b)->(Acomponent a->b)->b->TermWrapper a->b
foldTW f1 f2 f3 f4 f5 (TWcase t0 ps ts) = f1 t0 ps (map (foldTW f1 f2 f3 f4 f5) ts)
foldTW f1 f2 f3 f4 f5 (TWeta a eta) = f2 (foldTW f1 f2 f3 f4 f5 a) eta
foldTW f1 f2 f3 f4 f5 (TWsimple a) = f3 a
foldTW f1 f2 f3 f4 f5 (TWacomp a) = f4 a
foldTW f1 f2 f3 f4 f5 TWbottom = f5

foldTWM :: Monad m => (Term->[Pattern]->[b]->m b)->(b->Etai->m b)->(a->m b)->(Acomponent a->m b)->m b->TermWrapper a->m b
foldTWM f1 f2 f3 f4 f5 = foldTW (\t0 ps mbs->do bs<-sequence mbs; f1 t0 ps bs)
                                (\mb eta->do b<-mb; f2 b eta)
                                f3 f4 f5

mapTW::(a->b)->TermWrapper a->TermWrapper b
mapTW f = foldTW TWcase TWeta (TWsimple .f) (\a->TWacomp (wrapA (getVars a).mapTW f.unwrapA$ a)) TWbottom

mapTWacc:: [Boundvar] -> ([Boundvar]->a->(c,TermWrapper b))->([c]->c)->c->TermWrapper a->(c,TermWrapper b)
mapTWacc bvs f1 f2 f3 = foldTW (\t0 ps ->(\(cs',ts')->(f2 cs',TWcase t0 ps ts')).unzip)
                               (\(c,tw) e-> (c,TWeta tw e))
                               (f1 bvs)
                               (\a->(\(c,tw)-> (c,TWacomp (wrapA (getVars a) tw))).mapTWacc (getVars a) f1 f2 f3.unwrapA$ a)
                               (f3,TWbottom)

mapTWaccM::Monad m => [Boundvar] -> ([Boundvar] -> a->m (c,TermWrapper b))->([c]->c)->c->TermWrapper a->m (c,TermWrapper b)
mapTWaccM bvs f1 f2 f3 = foldTWM (\t0 ps ->(\(cs',ts')->return (f2 cs',TWcase t0 ps ts')).unzip)
                                 (\(c,tw) e-> return (c,TWeta tw e))
                                 (f1 bvs)
                                 (\a->do (c,tw)<-mapTWaccM (getVars a) f1 f2 f3.unwrapA$ a
                                         return (c,TWacomp (wrapA (getVars a) tw)))
                                 (return (f3,TWbottom))





type Coalgebra ca = ([Boundvar],[Term],ca)



newtype Psi = Psi [Psii]
newtype Psii = Psii PsiiRep
type PsiiRep = ([Pattern],[TupleTerm])



newtype OutF = OutF [OutFi]
newtype OutFi = OutFc (Constructor,[Variable],[TupleTerm])





newtype Sigma = Sigma ([Int],[[TupleTerm]],[[PatternS]],[Maybe (Int,[Acomponent InF],[Etai],WrappedCA,Int->Term->Term)])

























data PatternS = PcaseS Variable Pattern PatternS



           | PcaseSana Int Variable Pattern PatternS





           | PcaseR Int Variable Constructor [Variable] [(PatternS,[Variable])]









           | Ppattern Variable Pattern



           | Pdone



      deriving (Eq)
--      deriving (Show,Eq)

data WrappedCA = WCApsi (Coalgebra Psi)
               | WCAoutF (Coalgebra OutF)
               | WCAsigma (Coalgebra Sigma)





type Eta = [Etai]

type Phii = PhiiRep
newtype Etai = Etai (EtaiRep EtaOp)




class HasTerm e where




   getTerm :: e -> Term





   setTerm :: Term -> e -> e










getVars :: Acomponent a -> [Boundvar]
getVars (Acomp (vrs,_)) = vrs



unwrapA :: Acomponent a -> TermWrapper a
unwrapA (Acomp (_,a)) = a



wrapA :: [Boundvar] -> TermWrapper a -> Acomponent a
wrapA vrs a = Acomp (vrs,a)









bottom :: TermWrapper a
bottom = TWbottom








class CoalgebraTerm ca where



   getTerms :: ca -> [TupleTerm]
   setTerms :: [TupleTerm] -> ca -> ca





   getPatterns :: ca -> [Pattern]




   getPos :: ca -> Variable -> [Position]



   getPos ca v = map getPosition.filter (elem v.vars.getTerm).getTerms$ ca




data EtaOp = EOid



            | EOgeneral [Boundvar] [Term]



            | EOsust [Variable] [Term] [Boundvar]




            | EOlet [Term] [Pattern] [Variable] [Term]




class CEta e where



   leftCompose :: EtaOp -> e -> e



   rightCompose :: e -> EtaOp -> e



   compose :: e -> e -> e



   idEta :: e



   isIdEta :: e -> Bool



infixl 3  `rightCompose`
infixr 3  `leftCompose`
infixr 2  `compose`

class (HasTerm e) => CTupleElem e where



 getPosition :: e -> Position



 tupleterm :: Position -> Term -> e




 getPositions :: [e] -> Variable -> [Position]
 getPositions [] v = []
 getPositions (tt:tts) v | elem v.vars.getTerm$ tt = getPosition tt:getPositions tts v
                         | otherwise = getPositions tts v



 getTupletermsWithArgIndexes :: [e] -> Variable -> [(e,[Int])]
 getTupletermsWithArgIndexes [] v = []
 getTupletermsWithArgIndexes (tt:tts) v =
        let tsi = filter (elem v.vars.fst)$ zip (termToRecList (getTerm tt)) [0..]
         in if null tsi then getTupletermsWithArgIndexes tts v
              else (tt,map snd tsi):getTupletermsWithArgIndexes tts v
      where termToRecList (Ttuple True ts) = ts
            termToRecList t = [t]




type Position=Variable




data ParaFunctor = PFprod [ParaFunctor]
                 | PFid Position
                 | PFcnt Position
 deriving Show



class RecursivelyStructured a where




  isRec :: a -> Position -> Bool



  getRecIndex :: a -> Position -> Maybe Int



  getArgIndexes :: a -> Position -> [[Int]]



  makePosNR :: [Position] -> a -> a



  expandPositions :: [(ParaFunctor,Position)] -> a -> a




  expanded :: a -> Position -> ParaFunctor



  remapPositions :: [(Position,Int)] -> a -> a




class HasComponents a where



  getComponentTerms :: a -> [[TupleTerm]]



  renamePatternVars :: a -> IntState a





  wrapSigma :: Coalgebra a -> WrappedCA















type PhiiRep = Term





type EtaiRep eo= ([eo],[eo])




















newtype HFunctor = HF [(Position,Int,[[Int]],ParaFunctor)]
  deriving Show

data TupleTerm = Tterm Term Position
-- deriving (Eq,Show)
 deriving (Eq)




instance CTupleElem TupleTerm where
 getPosition (Tterm _ p) = p
 tupleterm p t=Tterm t p

foldPF :: (Variable->b)->(Variable->b)->([b]->b)->ParaFunctor->b
foldPF f1 f2 f3 (PFid v) = f1 v
foldPF f1 f2 f3 (PFcnt v) = f2 v
foldPF f1 f2 f3 (PFprod bvs) = f3 (map (foldPF f1 f2 f3) bvs)

foldPFM :: Monad m => (Variable->m b)->(Variable->m b)->([b]->m b)->ParaFunctor->m b
foldPFM f1 f2 f3 (PFid v) = f1 v
foldPFM f1 f2 f3 (PFcnt v) = f2 v
foldPFM f1 f2 f3 (PFprod bvs) = mapM (foldPFM f1 f2 f3) bvs >>= f3

instance Vars ParaFunctor where
 vars = foldPF (:[]) (:[]) concat



instance RecursivelyStructured HFunctor where
  isRec (HF vrs) p = any isR vrs
     where isR (p',_,_,bv) = p==p' && foldPF (const True) (const False) or bv || foldPF (==p) (const False) or bv
  getRecIndex (HF vrs) p = fmap (\(_,i,_,_)->i)$ find (findPos p)$ vrs
     where findPos p (p',_,_,bv) = p==p' && foldPF (const True) (const False) or bv || foldPF (p==) (const False) or bv
  getArgIndexes (HF vrs) p = maybe [] (\(_,_,iss,_)->iss)$ find (findPos p)$ vrs
     where findPos p (p',_,_,bv) = p==p' && foldPF (const True) (const False) or bv || foldPF (p==) (const False) or bv
  makePosNR ps (HF vrs) = HF (mknr' ps vrs)
     where mknr' ps vrs = if any (\(v,_,_,_)->elem v ps) vrs
                            then map (\t@(v,i,iss,bv)-> if elem v ps then (v,i,iss,foldPF PFcnt PFcnt PFprod bv) else t) vrs
                            else mknr ps vrs
           mknr ps (h@(p,i,iss,bv):vs) | any (flip elem ps) (vars bv) = (p,i,iss,foldPF (mknrbv ps) PFcnt PFprod bv):vs
                                       | otherwise = h:mknr ps vs
           mknr ps [] = []
           mknrbv ps p | elem p ps = PFcnt p
                       | otherwise = PFid p
  expandPositions ps (HF vrs) = HF (map (exp ps)  vrs)
     where exp ps (p,i,iss,bv) = (p,i,iss,foldPF (expbv ps) PFcnt PFprod bv)
            where  expbv ps v = case find ((v==).snd) ps of
                                 Just (PFprod [],_) -> PFid v
                                 Just (pf,_) -> pf
                                 _ -> PFid v
  expanded fnc@(HF vrs) p = case find (\(p',_,_,_)->p'==p) vrs of
                             Just (_,_,_,PFprod []) -> PFcnt p
                             Just (_,_,_,bv) -> bv
                             _ | isRec fnc p -> PFid p
                               | otherwise -> PFcnt p
  remapPositions idxs (HF vrs) = 
                   HF$ map (\o@(a,i,iss,b)->maybe o (\i'->(a,i',iss,b))$ 
                                            maybe (foldPF (flip lookup idxs) (const Nothing) findJust b) Just$
                                            lookup a idxs) 
                       vrs
     where findJust (Just a:as) = Just a
           findJust (_:as) = findJust as
           findJust _ = Nothing






mapStructure :: Term -> Term -> ParaFunctor -> Term
mapStructure f1 t2 = foldPF (const f1) (const t2) (Ttuple False)

instance HasTerm TupleTerm where
  getTerm (Tterm t _) = t
  setTerm t (Tterm  _ p) = Tterm t p





type FusionState a = ErrorT FusionError (State GenDict) a

runFusionState :: FusionState a -> Either FusionError a
runFusionState m = evalState (runErrorT m) emptyGenDict

data FusionError = 
             NotSaturated Term
             | NotExpected Term
             | NotDerivable
             | NotLegal
             | NotInF
             | NotOutF
             | NotTau
             | NotSigma
             | NotFound String
             | Msg String
--             | ParserError SrcLoc String

instance Error FusionError where
 noMsg = NotLegal
 strMsg = Msg

--instance Show FusionError where
-- show (NotSaturated t) = not_Satured t
-- show (NotExpected t) = not_Expected t
-- show NotDerivable = not_Derivable
-- show NotLegal = not_Legal_Term
-- show NotInF = not_InF_Term
-- show NotOutF = not_OutF_Term
-- show NotTau = right_Hylo_Not_Tau_Form
-- show NotSigma = left_Hylo_Not_Sigma_Form
-- show (NotFound s) = not_Found s
-- show (Msg chrs) = error_Message chrs
-- show (ParserError loc s) = showLoc loc ++ s
--  where showLoc l = srcFilename l ++ "("++ show (srcLine l) ++ ","++ show (srcColumn l)++"): "

class TermWrappable a where
 wrapTerm :: Term -> a
instance TermWrappable Phii where
 wrapTerm = id
instance TermWrappable InF where
 wrapTerm t = InF (" ",[t])
instance TermWrappable Tau where
 wrapTerm t = wrapTau (TWsimple (Tausimple t::TauTerm (Acomponent Phii)))


%%]
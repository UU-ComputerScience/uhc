-- Pretty printing library
-- based on papers by Olaf Chitil and Doaitse Swierstra
-- Version 1.0
-- 12/4/2007
--
-- Based on Wadler's pretty printing combinators.
-- Includes several additional useful combinators compared to the papers.
-- Implements basically a subset of Daan Leijen's library PPrint
-- http://www.cs.uu.nl/~daan/pprint.html
-- So see user documentation of PPrint.
-- Missing are the primitive combinators fill and fillBreak.
-- I don't need them and hence haven't thought about how to implement them.
-- Also missing are many derived combinators that I'm too lazy for.
--
-- Internally uses an algebraic data type for documents.
-- Could still be optimised further by specialisation of the interpreter
-- for dequeues of size 0, 1 and >1.

-- 20070416, Atze Dijkstra, adapted for use in EH.Util library


module EH.Util.Chitil.FPretty 
  (Doc,empty,text,line,linebreak,softline,softbreak,hardbreak
  ,(<>),(<+>),(<$>),(<$$>),(</>),(<//>)
  ,group,nest,align,hang,hsep,vsep,fillSep,sep,hcat,vcat,fillCat,cat
  , pretty
  ) where

import qualified EH.Util.Chitil.Dequeue as Dequeue

-- import Debug.Trace

infixr 6 <>,<+>
infixr 5 <$>,<$$>,</>,<//>

----------------------
-- derived combinators

softline :: Doc
softline = group line

softbreak :: Doc
softbreak = group linebreak

hardbreak :: Doc
hardbreak = Line 500  (take 500 (repeat ' '))

hang :: Int -> Doc -> Doc
hang i x = align (nest i x)

(<+>) :: Doc -> Doc -> Doc
dl <+> dr = dl <> text " " <> dr

(<$>) :: Doc -> Doc -> Doc
dl <$> dr = dl <> line <> dr

(<$$>) :: Doc -> Doc -> Doc
dl <$$> dr = dl <> linebreak <> dr

(</>) :: Doc -> Doc -> Doc
dl </> dr = dl <> softline <> dr

(<//>) :: Doc -> Doc -> Doc
dl <//> dr = dl <> softbreak <> dr

-- The following differ from PPrint in that they assume a non-empty list
-- and they do not start a new line at the end.

hsep :: [Doc] -> Doc
hsep = foldr1 (<+>)  -- differs from PPrint

vsep :: [Doc] -> Doc
vsep = foldr1 (<$>)  -- differs from PPrint

fillSep :: [Doc] -> Doc
fillSep = foldr1 (</>)  -- differs from PPrint

sep :: [Doc] -> Doc
sep xs = group (vsep xs)  -- differs from PPrint

hcat :: [Doc] -> Doc
hcat = foldr1 (<>)  -- differs from PPrint

vcat :: [Doc] -> Doc
vcat = foldr1 (<$$>)  -- differs from PPrint

fillCat :: [Doc] -> Doc
fillCat = foldr1 (<//>)   -- differs from PPrint

cat :: [Doc] -> Doc
cat xs = group (vcat xs)  -- differs from PPrint

-------------------
-- core combinators

-- the empty document; equal to text ""
empty :: Doc

-- atomic document consisting of just the given text
text :: String -> Doc

-- either a space or a new line
line :: Doc

-- either nothing (empty) or a new line
linebreak :: Doc

-- horizontal composition of two documents
(<>) :: Doc -> Doc -> Doc

-- mark document as group that is put into a single line if possible
group :: Doc -> Doc

-- increase indentation; assume >= 0
nest :: Int -> Doc -> Doc

-- set indentation to current column
align :: Doc -> Doc

-- pretty print within given width
pretty :: Int -> Doc -> String


data Doc = Text Int String  -- includes length of text string
         | Nil
         | Line Int String  -- includes length of optional text
         | Doc :<> Doc
         | Group Doc
         | Nest Int Doc     -- increase current indentation
         | Align Int Doc    -- set indentation to current column plus increment

empty = Nil
text t = Text (length t) t
line = Line 1 " "
linebreak = Line 0 ""
(<>) = (:<>)
group = Group
nest = Nest
align = Align 0
pretty w d = interpret (normalise d) w (\p dq r i -> "") 0 Dequeue.empty w 0


-- semantic-preserving transformation that ensures that between every end
-- of group and a subsequent line there is no text
normalise :: Doc -> Doc
normalise d = td :<> sd
  where
  (td,sd) = go d Nil
  -- Assume second argument only built from text,nil and <>.
  -- Ensures first component of result built only from text,nil and <>.
  -- go d tt = (td,sd) implies  d <> tt and td <> sd denote the same set of 
  -- layouts.
  go :: Doc -> Doc -> (Doc,Doc)
  go Nil tt = (tt,Nil)
  go (Text l t) tt = (Text l t :<> tt,Nil)
  go (Line l t) tt = (Nil,Line l t :<> tt)
  go (dl :<> dr) tt = let (tdl,sdl) = go dl tdr
                          (tdr,sdr) = go dr tt
                      in  (tdl,sdl :<> sdr)
  go (Group d) tt = let (td,sd) = go d tt in (td,Group sd)
  go (Nest i d) tt = let (td,sd) = go d tt in (td,Nest i sd)
  go (Align i d) tt = let (td,sd) = go d tt in (td,Align (i - docLength td) sd)

-- Determine length of a document consisting only of text,nil and <>.
-- To ensure linear complexity for align should actually keep track
-- of document length within go function itself.
docLength :: Doc -> Int
docLength Nil = 0
docLength (Text l _) = l
docLength (dl :<> dr) = docLength dl + docLength dr

type Width = Int
type Position =  Int
type Indentation = Int 
type Horizontal = Bool
type Remaining =  Int
type Out = Remaining -> Indentation -> String  
     -- indentation needed here because of align combinator
type OutGroup = Horizontal -> Out -> Out
type TreeCont = Position -> Dequeue.Seq (Position,OutGroup) -> Out


interpret :: Doc -> Width -> TreeCont -> TreeCont
interpret Nil w tc p ds = tc p ds
interpret (Text l t) w tc p ds =
  extendFrontGroup id prune outText tc (p+l) ds
  where
  outText :: OutGroup
  outText h c r i = t ++ c (r-l) i
interpret (Line l t) w tc p ds =
  extendFrontGroup id prune outLine tc (p+l) ds
  where
  outLine :: OutGroup
  outLine h c r i = if h then t ++ c (r-l) i
                         else '\n' : replicate i ' ' ++ c (w-i) i
interpret (dl :<> dr) w tc p ds =
  interpret dl w (interpret dr w tc) p ds
interpret (Group d) w tc p ds = 
  interpret d w (leaveGroup tc) p (Dequeue.cons (p,\h c -> c) ds)
interpret (Nest j d) w tc p ds =
  extendFrontGroup (interpret d w) (interpret d w) outNest tc p ds
  where
  outNest :: OutGroup
  outNest h c r i = c r (i+j)
interpret (Align j d) w tc p ds = 
  extendFrontGroup (interpret d w) (interpret d w) outAlign tc p ds
  where
  outAlign :: OutGroup
  outAlign h c r i = c r (w-r+j)


-- If no pending groups, then do out directly,
-- otherwise add out to pending group, applying given prune function.
-- This extracts an otherwise repeated pattern of the interpret function.
extendFrontGroup :: (TreeCont -> TreeCont) -> (TreeCont -> TreeCont) -> 
                    OutGroup -> TreeCont -> TreeCont
extendFrontGroup cont1 cont2 out tc p ds =
  case Dequeue.front ds of
    Nothing -> out False (cont1 tc p ds)
    Just ((s,outGrp),ds') -> 
      cont2 tc p (Dequeue.cons (s,\h c -> outGrp h (out h c)) ds')


leaveGroup :: TreeCont -> TreeCont
leaveGroup tc p ds = 
  case Dequeue.front ds of
    Nothing -> tc p ds
    Just ((s1,outGrp1),ds1) -> 
      case Dequeue.front ds1 of
        Nothing -> outGrp1 True (tc p Dequeue.empty)
        Just ((s2,outGrp2),ds2) ->
          tc p (Dequeue.cons (s2, \f c -> 
                outGrp2 f (\r1 -> outGrp1 (p <= s2+r1) c r1)) ds2)


prune :: TreeCont -> TreeCont
prune tc p ds = 
  case Dequeue.back ds of
    Nothing -> tc p ds
    Just ((s,outGrp),ds') -> \r -> if p > s+r 
                                     then outGrp False (prune tc p ds') r
                                     else tc p ds r


-- ---------------------------
-- For testing:

prop0 = pretty 6 (group (text "Hi" <> line <> text "you") <> text "!") ==
        "Hi\nyou!"
prop1 = pretty 4 (group (text "hi" <> line <> text "world")) ==
        "hi\nworld"
prop2 = 
  pretty 8 (group (text "hi" <> line <> text "world") <> text "liness") ==
  "hi\nworldliness"
prop3 = 
  take 6 (pretty 4 (group (text "hi" <> line <> text "you" <> undefined))) ==
  "hi\nyou"
prop4 = 
  take 6 (pretty 4 (group (text "hi" <> line) <>
           group (text "you" <> line) <> undefined)) ==
  "hi\nyou"
prop5 = 
  take 6 (pretty 4 (group (text "hi" <> 
           group (line <> text "you" <> undefined)))) ==
  "hi\nyou"
prop6 = 
  take 7 (pretty 3 (group (text "hi" <> line <> 
           group (line <> text "you" <> undefined)))) ==
  "hi\n\nyou"
prop7 = 
  pretty 10 (group (text "what" <>
    align (group (text "do" <> line <> text "you" <> line <> 
      text "do" <> align (line <> text "now?"))))) ==
  "whatdo\n    you\n    do\n      now?"

prop8 = 
  pretty 10 (group (text "one " <> (align (line <> text "two" <> 
    align (line <> text "three"))))) ==
  "one \n    two\n       three"

prop9 =
  pretty 10 (group (text "one " <> (nest 2 (line <> text "two" <>
    nest 3 (line <> text "three"))))) ==
  "one \n  two\n     three"
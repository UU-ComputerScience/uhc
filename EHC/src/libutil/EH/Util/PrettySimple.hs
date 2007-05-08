-------------------------------------------------------------------------
-- Subset of UU.Pretty, based on very simple pretty printing
-------------------------------------------------------------------------

module EH.Util.PrettySimple
  ( PP_Doc, PP(..)
  , disp
  , hPut
  
  , (>|<), (>-<)
  , (>#<)
  , hlist, vlist, hv
  , fill
  , indent

{-
  , pp_wrap, pp_quotes, pp_doubleQuotes, pp_parens, pp_brackets, pp_braces
  , ppPacked, ppParens, ppBrackets, ppBraces, ppCurlys
-}

  , empty, text
  )
  where

import IO

-------------------------------------------------------------------------
-- Doc structure
-------------------------------------------------------------------------

data Doc
  = Emp
  | Str			!String					-- basic string
  | Hor			Doc  !Doc				-- horizontal positioning
  | Ver			Doc  !Doc				-- vertical positioning
  | Ind			!Int Doc				-- indent

type PP_Doc = Doc

-------------------------------------------------------------------------
-- Basic combinators
-------------------------------------------------------------------------

infixr 3 >|<, >#<
infixr 2 >-< 

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r = pp l `Hor` pp r

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
l >-< r = pp l `Ver` pp r	-- pp l <$$> pp r

(>#<) :: (PP a, PP b) => a -> b -> PP_Doc
l >#< r  =  l >|< " " >|< r

indent :: PP a => Int -> a -> PP_Doc
indent i d = Ind i $ pp d

text :: String -> PP_Doc
text = Str

empty :: PP_Doc
empty = Emp

-------------------------------------------------------------------------
-- Derived combinators
-------------------------------------------------------------------------

hlist, vlist :: PP a => [a] -> PP_Doc
vlist [] = empty
vlist as = foldr  (>-<) empty as
hlist [] = empty
hlist as = foldr  (>|<) empty as

hv :: PP a => [a] -> PP_Doc
hv = vlist

fill :: PP a => [a] -> PP_Doc
fill = hlist

-------------------------------------------------------------------------
-- PP class
-------------------------------------------------------------------------

class Show a => PP a where
  pp     :: a   -> PP_Doc
  pp       = text . show

  ppList :: [a] -> PP_Doc
  ppList as = hlist as

instance PP PP_Doc where
  pp     = id

instance PP Char where
  pp c   = text [c]
  ppList = text

instance PP a => PP [a] where
  pp = ppList

instance Show PP_Doc where
  show p = disp p 200 ""

instance PP Int where
  pp = text . show

instance PP Float where
  pp = text . show

-------------------------------------------------------------------------
-- Observation
-------------------------------------------------------------------------

isEmpty :: PP_Doc -> Bool
isEmpty Emp         = True
isEmpty (Ver d1 d2) = isEmpty d1 && isEmpty d2
isEmpty (Hor d1 d2) = isEmpty d1 && isEmpty d2
isEmpty (Ind _  d ) = isEmpty d
isEmpty _           = False

-------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------

disp  ::  PP_Doc -> Int -> ShowS
disp d _ s
  = r
  where (r,_) = put 0 d s
        put p d s
          = case d of
              Emp              -> (s,p)
              Str s'           -> (s' ++ s,p + length s')
              Ind i  d         -> (ind ++ r,p')
                               where (r,p') = put (p+i) d s
                                     ind = replicate i ' '
              Hor d1 d2        -> (r1,p2)
                               where (r1,p1) = put p  d1 r2
                                     (r2,p2) = put p1 d2 s
              Ver d1 d2 | isEmpty d1
                               -> put p d2 s
              Ver d1 d2 | isEmpty d2
                               -> put p d1 s
              Ver d1 d2        -> (r1,p2)
                               where (r1,p1) = put p d1 $ "\n" ++ ind ++ r2
                                     (r2,p2) = put p d2 s
                                     ind = replicate p ' '

hPut  :: Handle -> PP_Doc -> Int -> IO ()
hPut h d _
  = do _ <- put 0 d h
       return ()
  where put p d h
          = case d of
              Emp              -> return p
              Str s            -> do hPutStr h s
                                     return $ p + length s
              Ind i  d         -> do hPutStr h $ replicate i ' '
                                     put (p+i) d h
              Hor d1 d2        -> do p' <- put p d1 h
                                     put p' d2 h
              Ver d1 d2 | isEmpty d1
                               -> put p d2 h
              Ver d1 d2 | isEmpty d2
                               -> put p d1 h
              Ver d1 d2        -> do _ <- put p d1 h
                                     hPutStr h $ "\n" ++ replicate p ' '
                                     put p d2 h

-------------------------------------------------------------------------
-- Common stuff w.r.t. attr
-------------------------------------------------------------------------

%%[1 hs module (AttrProps)
%%]

%%[1 hs export(AtDir, AtProp(..), propsDir)
%%]

%%[1 hs import (Data.Set)
%%]

%%[1 hs import (EH.Util.Pretty)
%%]

%%[1 hs

-------------------------------------------------------------------------
-- Attr props/dir/info
-------------------------------------------------------------------------

{-
data AtDir
  = AtInh | AtSyn | AtIn | AtOut | AtInOut
  deriving (Eq,Ord,Show)
-}

type AtDir = AtProp

propsDir :: Set AtProp
propsDir = fromList [ AtInh, AtSyn, AtIn, AtOut, AtInOut ]

data AtProp
  = AtNode | AtThread | AtUpdown | AtRetain | AtExtern | AtInh | AtSyn | AtIn | AtOut | AtInOut
  deriving (Eq,Ord,Show)

{-
instance PP AtDir where
  pp = text . show
-}

instance PP AtProp where
  pp = text . show

%%]

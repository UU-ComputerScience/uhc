-------------------------------------------------------------------------
-- Common stuff w.r.t. attr
-------------------------------------------------------------------------

module AttrProps
  ( AtDir {- (..) -}, AtProp(..)
  , propsDir
  )
  where

import Data.Set
import UU.Pretty

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


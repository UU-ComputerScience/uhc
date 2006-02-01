-------------------------------------------------------------------------
-- Common stuff w.r.t. attr
-------------------------------------------------------------------------

module AttrProps
  ( AtDir(..), AtProp(..)
  )
  where

import UU.Pretty

-------------------------------------------------------------------------
-- Attr props/dir/info
-------------------------------------------------------------------------

data AtDir
  = AtInh | AtSyn | AtIn | AtOut | AtInOut
  deriving (Eq,Ord,Show)

data AtProp
  = AtNode | AtThread | AtUpdown | AtRetain
  deriving (Eq,Ord,Show)

instance PP AtDir where
  pp = text . show

instance PP AtProp where
  pp = text . show


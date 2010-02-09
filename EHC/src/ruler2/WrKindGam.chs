-------------------------------------------------------------------------
-- WrKind Gamma
-------------------------------------------------------------------------

%%[1 hs module (WrKindGam)
%%]

%%[1 hs export (module Gam, WrKindInfo(..), WrKindGam, wrKindGam)
%%]

%%[1 hs import (qualified Data.Map as Map, EH.Util.Pretty, Common, Gam)
%%]

-------------------------------------------------------------------------
-- WrKind
-------------------------------------------------------------------------

%%[1 hs

data WrKindInfo
  = WrKindInfo
      { wkBegCmd   :: Nm
      , wkEndCmd   :: Nm
      }

instance Show WrKindInfo where
  show _ = "WrKindInfo"

instance PP WrKindInfo where
  pp i = pp "WK"

type WrKindGam = Gam WrKind WrKindInfo

wrKindGam :: WrKindGam
wrKindGam
  = gamFromAssocs
      [ (WrIsChanged,WrKindInfo nmCmdBegChng nmCmdEndChng)
      , (WrIsSame   ,WrKindInfo nmCmdBegSame nmCmdEndSame)
      ]

%%]

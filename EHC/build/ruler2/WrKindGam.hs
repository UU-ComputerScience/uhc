module WrKindGam
( module Gam, WrKindInfo (..), WrKindGam, wrKindGam )
where
import qualified Data.Map as Map
import EH.Util.Pretty
import Common
import Gam


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


module EH101.Base.ParseUtils
( P
, parseString )
where
import EH101.Base.Common
import UU.Parsing
import EH.Util.ParseUtils
import EH.Util.ScanUtils
import EH101.Scanner.Common
import EH101.Base.HsName
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.Version
import Data.List

{-# LINE 34 "src/ehc/Base/ParseUtils.chs" #-}
type P p = PlainParser Token p

{-# LINE 42 "src/ehc/Base/ParseUtils.chs" #-}
parseString :: ScanOpts -> P res -> String -> Maybe res
parseString scanOpts p s
  = if null errs then Just res else Nothing
  where tokens     = scan scanOpts (initPos s) s
        (res,errs) = parseToResMsgs p tokens

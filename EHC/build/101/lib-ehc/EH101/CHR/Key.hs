module EH101.CHR.Key
( Key (..)
, TTKeyableOpts (..), defaultTTKeyableOpts
, TTKeyable (..)
, toTTKey )
where
import EH101.Base.Common
import EH101.Base.TreeTrie
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Ty
import EH101.Ty.Pretty
import Data.Typeable (Typeable)
import Data.Generics (Data)
import EH101.Base.Serialize
import Control.Monad
import EH101.Base.Hashable
import Data.Bits




{-# LINE 35 "src/ehc/CHR/Key.chs" #-}
data Key
  = Key_HNm     !HsName         -- type constant, its name
  | Key_UID     !UID            -- type variable, its id, used with TKK_Partial
  | Key_Str     !String         -- arbitrary string
  | Key_TyQu    !TyQu           -- quantified type, used with TKK_Partial
  | Key_Ty      !Ty             -- catchall for the rest, used with TKK_Partial
  deriving ( Eq, Ord
           , Typeable, Data
           )

{-# LINE 59 "src/ehc/CHR/Key.chs" #-}
instance Show Key where
  show (Key_HNm  n) = "H:" ++ show n
  show (Key_UID  n) = "U:" ++ show n
  show (Key_Str  n) = "S:" ++ n
  show (Key_TyQu n) = "Q:" ++ show n
  show (Key_Ty   n) = "T:" ++ show n

{-# LINE 73 "src/ehc/CHR/Key.chs" #-}
instance PP Key where
  pp (Key_HNm  n) = "H:" >|< n
  pp (Key_UID  n) = "U:" >|< n
  pp (Key_Str  n) = "S:" >|< n
  pp (Key_TyQu n) = "Q:" >|< show n
  pp (Key_Ty   n) = "T:" >|< n

{-# LINE 140 "src/ehc/CHR/Key.chs" #-}
data TTKeyableOpts
  = TTKeyableOpts
      { ttkoptsVarsAsWild       :: Bool             -- treat vars as wildcards
      }

defaultTTKeyableOpts = TTKeyableOpts True

{-# LINE 149 "src/ehc/CHR/Key.chs" #-}
-- | TreeTrie key construction
class TTKeyable x where
  toTTKey'                  :: TTKeyableOpts -> x ->  TreeTrieKey  Key                          -- option parameterized constuction
  toTTKeyParentChildren'    :: TTKeyableOpts -> x -> (TreeTrie1Key Key, [TreeTrieMpKey  Key])   -- building block: parent of children + children

  -- default impl
  toTTKey' o                    = uncurry ttkAdd' . toTTKeyParentChildren' o
  toTTKeyParentChildren' o      = ttkParentChildren . toTTKey' o

{-# LINE 160 "src/ehc/CHR/Key.chs" #-}
toTTKey :: TTKeyable x => x -> TreeTrieKey Key
toTTKey = toTTKey' defaultTTKeyableOpts

{-# LINE 165 "src/ehc/CHR/Key.chs" #-}
instance TTKeyable x => TreeTrieKeyable x Key where
  toTreeTrieKey   = toTTKey

{-# LINE 174 "src/ehc/CHR/Key.chs" #-}
instance Serialize Key where
  sput (Key_HNm  a) = sputWord8 0 >> sput a
  sput (Key_UID  a) = sputWord8 1 >> sput a
  sput (Key_Str  a) = sputWord8 2 >> sput a
  sput (Key_TyQu a) = sputWord8 3 >> sput a
  sput (Key_Ty   a) = sputWord8 4 >> sput a
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  Key_HNm  sget
      1 -> liftM  Key_UID  sget
      2 -> liftM  Key_Str  sget
      3 -> liftM  Key_TyQu sget
      4 -> liftM  Key_Ty   sget


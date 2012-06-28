module EH101.Base.Strictness
( Strictness (..) )
where
import EH101.Base.HsName
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize




{-# LINE 20 "src/ehc/Base/Strictness.chs" #-}
data Strictness
  = Strictness_Strict
  | Strictness_NonStrict
  | Strictness_Var HsName
  deriving (Eq, Ord)

instance Show Strictness where
  show Strictness_Strict    = "strict"
  show Strictness_NonStrict = "nonStrict"
  show (Strictness_Var n)   = "strictness:" ++ show n

{-# LINE 38 "src/ehc/Base/Strictness.chs" #-}
deriving instance Typeable Strictness
deriving instance Data Strictness

{-# LINE 47 "src/ehc/Base/Strictness.chs" #-}
instance Binary Strictness where
  put (Strictness_Strict           )   = putWord8 0
  put (Strictness_NonStrict        )   = putWord8 1
  put (Strictness_Var          nm  )   = putWord8 2 >> put nm
  get = do  t <- getWord8
            case t of
              0 -> return Strictness_Strict
              1 -> return Strictness_NonStrict
              2 -> liftM  Strictness_Var get

instance Serialize Strictness where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain



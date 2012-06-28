module EH101.Ty.FIEnv2
( defaultFIEnv )
where
import EH101.Ty.FIEnv
import EH101.Ty.AppSpineGam
import EH101.AbstractCore


{-# LINE 25 "src/ehc/Ty/FIEnv2.chs" #-}
defaultFIEnv
  =   emptyFE
        { feAppSpineGam = mkAppSpineGam defaultFIEnv
        }


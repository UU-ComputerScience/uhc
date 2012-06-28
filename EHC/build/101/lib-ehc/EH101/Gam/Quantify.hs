module EH101.Gam.Quantify
( tyKiGamQuantifyWithVarMp
, valGamQuantify
, valGamQuantifyWithVarMp
, quantifyPolGam )
where
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import qualified Data.Set as Set
import EH101.Ty.Trf.Quantify
import EH101.VarMp
import EH101.Substitutable
import EH101.Gam
import EH101.Gam.ValGam
import EH101.Gam.TyKiGam
import EH101.Ty.Trf.MergePreds
import EH101.Gam.PolGam






{-# LINE 57 "src/ehc/Gam/Quantify.chs" #-}
valGamQuantify :: TyVarIdS -> [PredOcc] -> ValGam -> (ValGam,TQOGam)
valGamQuantify globTvS prL g
  =  let  g' = gamMapElts  (\vgi ->  let  tmpo = tyMergePreds prL (vgiTy vgi)
                                          ty   = valTyQuantify (const kiStar) (`Set.member` globTvS) (tmpoTy tmpo)
                                     in   (vgi {vgiTy = ty},tmpo {tmpoTy = ty})
                           ) g
     in   gamUnzip g'

{-# LINE 67 "src/ehc/Gam/Quantify.chs" #-}
valGamQuantifyWithVarMp :: Bool -> TyKiGam -> VarMp -> VarMp -> TyVarIdS -> [PredOcc] -> ValGam -> (ValGam,VarMp,(VarMp,TQOGam))
valGamQuantifyWithVarMp doQuant tyKiGam tvKiVarMp gamVarMp globTvS prL valGam
  = valGamDoWithVarMp quant gamVarMp (emptyVarMp,emptyGam) valGam
  where quant nm (t,tyCycVarMp) newVarMp (cycVarMp,tmpoGam)
          = ( ty
            , newVarMp
            , (tyCycVarMp `varUpd` cycVarMp
              , gamAdd nm (tmpo {tmpoTy = ty}) tmpoGam
            ) )
          where tmpo           = tyMergePreds prL t
                ty | doQuant   = valTyQuantify (tvarKi tyKiGam tvKiVarMp gamVarMp) (`Set.member` globTvS) (tmpoTy tmpo)
                   | otherwise = tmpoTy tmpo

{-# LINE 95 "src/ehc/Gam/Quantify.chs" #-}
tyKiGamQuantifyWithVarMp :: {- TyKiGam -> VarMp -> -} VarMp -> TyVarIdS -> TyKiGam -> (TyKiGam,VarMp,VarMp)
tyKiGamQuantifyWithVarMp {- tyKiGam tvKiVarMp -} gamVarMp globTvS gam
  = tyKiGamDoWithVarMp
      (\_ (t,tyCycMp) m cycMp -> (tyKiQuantify {- (tvarKi tyKiGam tvKiVarMp gamVarMp) -} (`Set.member` globTvS) t,m,tyCycMp `varUpd` cycMp))
      gamVarMp emptyVarMp gam

{-# LINE 107 "src/ehc/Gam/Quantify.chs" #-}
quantifyPolGam :: PolGam -> PolGam
quantifyPolGam gam
  = let fvs = varFree gam
        notElemFtvs tv = not $ elem tv fvs
     in mapPolGam (tyQuantify notElemFtvs) gam


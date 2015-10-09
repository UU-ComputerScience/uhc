%%[0 hs
{-# LANGUAGE TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environmental info required during code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environmental info required during code generation
%%]

%%[8 hs module {%{EH}CodeGen.CEnv}
%%]

%%[8 hs import(Data.Typeable)
%%]
%%[8 hs import(UHC.Util.Lens)
%%]

%%[8 hs import({%{EH}Gam}, {%{EH}Gam.DataGam})
%%]
%%[(8 core) hs import({%{EH}LamInfo})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment info for codegen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(CEnv(..), emptyCEnv)
data CEnv
  = CEnv
      { _cenvDataGam         :: DataGam			-- info about datatypes
      											-- 20151009 AD: TBD, for now non strict field
%%[[(8 core)
      , _cenvLamMp         	 :: LamMp			-- info about functions
      											-- 20151009 AD: TBD, for now non strict field
%%]]
      }
  deriving (Typeable)

emptyCEnv =
  CEnv emptyGam
%%[[(8 core)
       emptyLamMp
%%]]
%%]

%%[8 export(cenvDataGam)
mkLabel ''CEnv
%%]

%%[(8 core) export(cenvLamMp)
%%]

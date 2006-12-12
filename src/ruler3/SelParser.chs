-------------------------------------------------------------------------
-- Sel parser
-------------------------------------------------------------------------

%%[1 hs module (SelParser)
%%]

%%[1 hs export (pSel1, pSel)
%%]

%%[1 hs import (UU.Parsing, Scanner, EH.Util.ParseUtils, Config( cfgStrSel ))
%%]

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

%%[1 hs

pSel1 :: (IsParser p Token) => (a1 -> t, a1 -> a -> a1, a2 -> a) -> (p a2, p a) -> p (a1 -> t)
pSel1 (top,sel,jst) (pE,pMbE)
  =   (\ss s -> \e -> top (sel (ss e) (jst s))) <$> pDots <*> pE
  where pSel' = flip sel <$> pMbE
        pDots = pChainr_ng ((\s -> \_ r -> \e -> r (s e)) <$> pSel') (id <$ pKey cfgStrSel)

pSel alg ps = pSel1 alg ps <|> pSucceed id

%%]

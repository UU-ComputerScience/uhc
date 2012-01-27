%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Basic token parsers, for constants and other simple stuff.

Note that everything is exported.
%%]

%%[1 module {%{EH}Scanner.TokenParser} import(UU.Parsing.Interface(IsParser(..)),UU.Parsing.Derived(pListSep, pPacked),UU.Scanner.Position(Pos),UU.Scanner.GenTokenParser)
%%]

%%[1.Token import(UU.Scanner.GenToken)
%%]

%%[5 -1.Token import({%{EH}Scanner.Token})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Token parser wrappers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pHsValToken
pHsValToken :: IsParser p (GenToken key tp val) => tp -> val -> p (val,Pos)
pHsValToken = pValToken

pHsCostValToken'          :: IsParser p (GenToken key tp val) => Int -> tp -> val -> p (GenToken key tp val)
pHsCostValToken' = pCostValToken'
%%]

%%[5 -1.pHsValToken
pHsValToken :: IsParser p Token => EnumValToken -> String -> p (String,Pos)
pHsValToken t s = valTokenStringRes <$> pValToken t [s]

pHsCostValToken'          :: IsParser p Token => Int -> EnumValToken -> String -> p Token
pHsCostValToken' c tp val =  pCostValToken' c tp [val]
%%]

%%[5
-------------------------------------------------------------------------
-- IsParsers for  Symbols
-------------------------------------------------------------------------

valTokenStringRes :: (ValTokenVal,Pos) -> (String,Pos)
valTokenStringRes (l,p) = (concat l,p)

pKeyPos           :: IsParser p Token => String -> p Pos
pKeyPos  keyword  =  pReserved keyword


pSpecPos          :: IsParser p Token => Char -> p Pos
pSpecPos s        =  pReserved [s]

pKey              :: IsParser p Token => String -> p String
pKey  key         =  key <$ pKeyPos key

pSpec             :: IsParser p Token => Char -> p String 
pSpec c           =  [c] <$ pSpecPos c
      
pStringPos, pCharPos,
  pInteger8Pos, pInteger10Pos, pInteger16Pos, pFractionPos,
  pVaridPos, pConidPos, pVarsymPos, pConsymPos,
%%[[18
  pVaridUnboxedPos, pConidUnboxedPos, pVarsymUnboxedPos, pConsymUnboxedPos,
%%]]
  pTextnmPos, pTextlnPos, pIntegerPos  :: IsParser p Token => p (String,Pos)

pStringPos            =   pHsValToken TkString           ""        
pCharPos              =   pHsValToken TkChar             "\NUL"    
pInteger8Pos          =   pHsValToken TkInteger8         "0"       
pInteger10Pos         =   pHsValToken TkInteger10        "0"       
pInteger16Pos         =   pHsValToken TkInteger16        "0"
pFractionPos          =   pHsValToken TkFraction         "0.0"
pVaridPos             =   pHsValToken TkVarid            "<identifier>" 
pConidPos             =   pHsValToken TkConid            "<Identifier>" 
pConsymPos            =   pHsValToken TkConOp 	       "<conoperator>"
pVarsymPos            =   pHsValToken TkOp               "<operator>" 
%%[[18
pVaridUnboxedPos      =   pHsValToken TkVaridUnboxed     "<identifier#>" 
pConidUnboxedPos      =   pHsValToken TkConidUnboxed     "<Identifier#>" 
pConsymUnboxedPos     =   pHsValToken TkConOpUnboxed 	   "<conoperator#>"
pVarsymUnboxedPos     =   pHsValToken TkOpUnboxed        "<operator#>" 
%%]]
pTextnmPos            =   pHsValToken TkTextnm           "<name>"       
pTextlnPos            =   pHsValToken TkTextln           "<line>"     
pIntegerPos           =   pInteger10Pos

pString, pChar,
  pInteger8, pInteger10, pInteger16, pFraction,
  pVarid, pConid, pVarsym, pConsym,
%%[[18
  pVaridUnboxed, pConidUnboxed, pVarsymUnboxed, pConsymUnboxed,
%%]]
  pTextnm, pTextln, pInteger  :: IsParser p Token => p String

pString        = fst <$> pStringPos        
pChar          = fst <$> pCharPos          
pInteger8      = fst <$> pInteger8Pos      
pInteger10     = fst <$> pInteger10Pos     
pInteger16     = fst <$> pInteger16Pos     
pFraction      = fst <$> pFractionPos     
pVarid         = fst <$> pVaridPos         
pConid         = fst <$> pConidPos         
pVarsym        = fst <$> pVarsymPos  
pConsym        = fst <$> pConsymPos 
%%[[18
pVaridUnboxed  = fst <$> pVaridUnboxedPos         
pConidUnboxed  = fst <$> pConidUnboxedPos         
pVarsymUnboxed = fst <$> pVarsymUnboxedPos  
pConsymUnboxed = fst <$> pConsymUnboxedPos 
%%]]
pTextnm        = fst <$> pTextnmPos       
pTextln        = fst <$> pTextlnPos            
pInteger       = fst <$> pIntegerPos       
  
pComma, pSemi, pOParen, pCParen, pOBrack, pCBrack, pOCurly, pCCurly
   :: IsParser p Token => p String

pComma  = pSpec ','
pSemi   = pSpec ';'
pOParen = pSpec '('
pCParen = pSpec ')'
pOBrack = pSpec '['
pCBrack = pSpec ']'
pOCurly = pSpec '{'
pCCurly = pSpec '}'

pCommaPos, pSemiPos, pOParenPos, pCParenPos, pOBrackPos, pCBrackPos, pOCurlyPos, pCCurlyPos
   :: IsParser p Token => p Pos

pCommaPos  = pSpecPos ','
pSemiPos   = pSpecPos ';'
pOParenPos = pSpecPos '('
pCParenPos = pSpecPos ')'
pOBrackPos = pSpecPos '['
pCBrackPos = pSpecPos ']'
pOCurlyPos = pSpecPos '{'
pCCurlyPos = pSpecPos '}'

pCommas ::  IsParser p Token => p a -> p [a]
pSemics ::  IsParser p Token => p a -> p [a]
pParens ::  IsParser p Token => p a -> p a
pBracks ::  IsParser p Token => p a -> p a
pCurly  ::  IsParser p Token => p a -> p a

pCommas  = pListSep pComma
pSemics  = pListSep pSemi
pParens  = pPacked pOParen pCParen
pBracks  = pPacked pOBrack pCBrack
pCurly   = pPacked pOCurly pCCurly

pParens_pCommas :: IsParser p Token => p a -> p [a]
pBracks_pCommas :: IsParser p Token => p a -> p [a]
pCurly_pSemics  :: IsParser p Token => p a -> p [a]

pParens_pCommas = pParens.pCommas
pBracks_pCommas = pBracks.pCommas
pCurly_pSemics  = pCurly .pSemics

%%]


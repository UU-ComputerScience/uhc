-------------------------------------------------------------------------
-- Chunk Parser
-------------------------------------------------------------------------

module ChunkParser
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import IO

import UU.Parsing
-- import UU.Parsing.CharParser
import UU.Scanner.Position( initPos, Pos )

import EH.Util.ScanUtils
import EH.Util.ParseUtils
import EH.Util.Utils

import Common
import MainAG

-------------------------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------------------------

type ScanOptsMp = Map.Map ScState ScanOpts

chKindMp = Map.fromList
             [ ("hs",ChHS), ("ag",ChAG), ("plain",ChPlain), ("haddock",ChHaddock) ]
           `Map.union` Map.fromList [ (v,k) | (k,v) <- Map.toList t2tChKinds ]
chDestMp = Map.fromList [ ("here",ChHere), ("hide",ChHide) ]
chWrapMp = Map.fromList [ ("code",ChWrapCode), ("safecode",ChWrapBoxCode Nothing), ("tt",ChWrapTT), ("tttiny",ChWrapTTtiny) , ("verbatim",ChWrapVerbatim), ("verbatimsmall",ChWrapVerbatimSmall) ]

kwTxtAsVarTooA
  = [ "module", "import", "export", "wrap", "ghc" ]
    ++ Map.keys chKindMp

kwTxtAsVarTooB
  = kwTxtAsVarTooA
    ++ Map.keys chDestMp
    ++ Map.keys chWrapMp
    ++ [ "beamerblockcode", "boxcode" ]

shuffleScanOpts :: ScanOptsMp
shuffleScanOpts
  = Map.fromList
        [ ( ScLexMeta 0
          , defaultScanOpts
              { scoKeywordsTxt      =   Set.fromList (kwTxtAsVarTooB ++ [ "_", "-", ".", "<", "=", "@", "||", "&&" ])
              , scoSpecChars        =   Set.fromList "(),%{}"
              , scoOpChars          =   Set.fromList "+-=*&^$#@!\\|><~`;:?/_."
              }
          )
        ]

data ScState
  = ScChunk Int | ScLexMeta Int | ScInline Int | ScSkip
  deriving Show

instance Eq ScState where
  ScChunk   _ == ScChunk   _ = True
  ScLexMeta _ == ScLexMeta _ = True
  ScInline  _ == ScInline  _ = True
  ScSkip      == ScSkip      = True
  _           == _           = False

instance Ord ScState where
  ScChunk   _ < ScLexMeta _ = True
  ScChunk   i < s           = ScLexMeta i < s
  ScLexMeta _ < ScInline  _ = True
  ScLexMeta i < s           = ScInline  i < s
  ScInline  _ < ScSkip      = True
  _           < _           = False

data TokKind
  = TkBegChunk  | TkEndChunk
  | TkBegInline | TkEndInline
  | TkBegExpand | TkEndExpand
  | TkBegGroup  | TkElseGroup
  | TkNameRef
  | TkReserved
  | TkNl   | TkEOF
  | TkText | TkInt | TkStr
  deriving (Show,Eq,Ord)

data Tok
  = Tok { tokKind :: TokKind, tokWhite :: String, tokBlack :: String, tokPos :: InFilePos, tokState :: ScState }

instance Eq Tok where
  (Tok k1 _ b1 _ _) == (Tok k2 _ b2 _ _) = k1 == k2 && (k1 /= TkReserved || b1 == b2)

instance Ord Tok where
  (Tok k1 _ b1 _ _) `compare` (Tok k2 _ b2 _ _)
    = if ck == EQ
      then if k1 == TkReserved then b1 `compare` b2 else ck
      else ck
    where ck = k1 `compare` k2

instance Show Tok where
  show t = show (tokPos t) ++ show (tokBlack t)

instance Symbol Tok

mbTokWhite :: Tok -> Maybe String
mbTokWhite (Tok _ w _ _ _) = if null w then Nothing else Just w

scan :: ScanOptsMp -> ScState -> String -> [Tok]
scan scoMp st s
  = takeWhile ((/=TkEOF) . tokKind) (sc infpStart st s)
  where sc p st               ""                      = [Tok TkEOF "" "" p st]
        sc p st@(ScChunk _)   s@(c:_)
          | isWhite c                               = t {tokWhite = w} : ts
                                                    where (w,s') = span isWhite s
                                                          (t:ts) = sc (a w p) st s'
        sc p st@(ScLexMeta l) ('%':'%':'}':s')      = Tok TkEndExpand  "" "%%}"  p st : sc (ai 3 p) (ScChunk l) s'
        sc p st@(ScLexMeta _) s@(c:_)
          | isWhite c                               = sc (a w p) st s'
                                                    where (w,s') = span isWhite s
        sc p st               s@(c:s')
          | isLF c                                  = Tok TkNl       ""   [c] p st : sc (al p) st' s'
                                                    where st' = case st of
                                                                  ScLexMeta l -> ScChunk l
                                                                  _           -> st
        sc p st@(ScLexMeta _) s@(c:s')
          | isSpec st c                             = Tok TkReserved ""   [c] p st : sc (ai 1 p) st s'
        sc p st@(ScLexMeta _) s@(c:_)
          | isVarStart c                            = scKw isVarRest p st s
        sc p st@(ScLexMeta _) s@(c:_)
          | isDigit c                               = Tok TkInt      ""   w   p st : sc (a w p) st s'
                                                    where (w,s') = span isDigit s
        sc p st@(ScLexMeta _) s@(c:s')
          | isStrQuote c                            = Tok TkStr      ""   w   p st : sc (a w . ai 2 $ p) st s3
                                                    where (w,s2) = span isStr s'
                                                          s3 = case s2 of
                                                                 (c:s) | isStrQuote c -> s
                                                                 _                    -> s2
        sc p st@(ScLexMeta _) s@(c:_)
          | isOpch st c                             = scKw (isOpch st) p st s
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':'[':'[':s')
                                                    = Tok TkBegGroup   "" "%%[[" p st : sc (ai 4 p) (ScLexMeta (l+1)) s'
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':']':'[':s')
                                                    = Tok TkElseGroup  "" "%%][" p st : sc (ai 4 p) (ScLexMeta (l)) s'
        sc p@(InFilePos _ 1) ScSkip            ('%':'%':'[':s')
                                                    = Tok TkBegChunk   "" "%%["  p ScSkip  : sc (ai 3 p) (ScLexMeta 0) s'
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':']':']':s')
          | l >  0                                  = Tok TkEndChunk   "" "%%]]" p st : sc (ai 4 p) (ScChunk (l-1)) s'
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':']':s')
          | l == 0                                  = Tok TkEndChunk   "" "%%]"  p st : sc (ai 3 p) ScSkip s'
          | l >  0                                  = Tok TkEndChunk   "" "%%]"  p st : sc (ai 3 p) (ScChunk (l-1)) s'
        sc p@(InFilePos _ _) st@(ScChunk l)    ('%':'%':'@':'[':s')
                                                    = Tok TkBegInline  "" "%%@[" p st : sc (ai 4 p) (ScInline l) s'
        sc p@(InFilePos _ _) st@(ScChunk l)    ('%':'%':'@':'{':s')
                                                    = Tok TkBegExpand  "" "%%@{" p st : sc (ai 4 p) (ScLexMeta l) s'
        sc p@(InFilePos _ _) st@(ScChunk l)    ('%':'%':'%':s)
                                                    = Tok TkText       "" b'     p st : sc (ai (1 + length b') p) (ScChunk l) s'
                                                    where (b,s') = span isBlack s
                                                          b'     = "%%" ++ b
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':'@':s')
                                                    = Tok TkNameRef    "" "%%@"  p st : sc (ai 3 p) (ScLexMeta l) s'
        sc p@(InFilePos _ _) st@(ScInline l)   ('%':'%':']':s')
                                                    = Tok TkEndInline  "" "%%]"  p st : sc (ai 3 p) (ScChunk l) s'
        sc p@(InFilePos _ _) st@(ScInline l)   s       = Tok TkText       "" il     p st : sc (a il p) (ScInline l) s'
                                                    where (il,s') = spanInline s
                                                          spanInline s@('%':'%':']':_) = ("",s)
                                                          spanInline ""                = ("","")
                                                          spanInline (c:s)             = let (b,e) = spanInline s in (c:b,e)
        sc p st@(ScChunk _) s@(c:_)
          | isBlack c && not (isInline s)           = Tok TkText       "" b      p st : sc (a b p) st s'
                                                    where (b,s') = span' (\s@(c:_) -> isBlack c && not (isInline s)) s
        sc p st             s@(c:s')                = sc (ai 1 p) st s'
        scKw f p st s                               = Tok tk           "" w      p st : sc (a w p) st s'
                                                    where (w,s') = span f s
                                                          tk = if isKeyw st w then TkReserved else TkText
        a                                           = infpAdvStr
        ai                                          = infpAdvCol
        al                                          = infpAdv1Line
        opt st p                                    = maybe False p $ Map.lookup st scoMp
        isSpec st c                                 = opt st (\o -> c `Set.member` scoSpecChars o)
        isOpch st c                                 = opt st (\o -> c `Set.member` scoOpChars o)
        isKeyw st w                                 = opt st (\o -> w `Set.member` scoKeywordsTxt o)
        isInline  ('%':'%':'@':'[':_)               = True
        isInline  ('%':'%':'@':'{':_)               = True
        isInline  ('%':'%':'%':    _)               = True
        isInline  _                                 = False
        span'                                       = spanOnRest

-------------------------------------------------------------------------
-- Parsers directly related to scanning
-------------------------------------------------------------------------

pBegChunk, pEndChunk
 , pBegInline, pEndInline
 , pBegExpand, pEndExpand
 , pBegGroup, pElseGroup
 , pBegNameRef, pNl :: (IsParser p Tok) => p Tok
pBegChunk   = pSym (Tok TkBegChunk  "" "%%["  infpNone ScSkip)
pEndChunk   = pSym (Tok TkEndChunk  "" "%%]"  infpNone ScSkip)
pBegInline  = pSym (Tok TkBegInline "" "%%@[" infpNone ScSkip)
pEndInline  = pSym (Tok TkEndInline "" "%%]"  infpNone ScSkip)
pBegExpand  = pSym (Tok TkBegExpand "" "%%@{" infpNone ScSkip)
pEndExpand  = pSym (Tok TkEndExpand "" "%%}"  infpNone ScSkip)
pBegGroup   = pSym (Tok TkBegGroup  "" "%%[[" infpNone ScSkip)
pElseGroup  = pSym (Tok TkElseGroup "" "%%][" infpNone ScSkip)
pBegNameRef = pSym (Tok TkNameRef   "" "%%@"  infpNone ScSkip)
pNl         = pSym (Tok TkNl        "" "LF"   infpNone ScSkip)

pKey :: (IsParser p Tok) => String -> p String
pKey k = tokBlack <$> pSym (Tok TkReserved "" k infpNone ScSkip)

pVar :: (IsParser p Tok) => p String
pVar = tokBlack <$> pSym (Tok TkText "" "<ident>" infpNone ScSkip)

pInt :: (IsParser p Tok) => p String
pInt = tokBlack <$> pSym (Tok TkInt "" "0" infpNone ScSkip)

pFrac :: (IsParser p Tok) => p String
pFrac
  = (++) <$> (pMaybe "" id pInt)
         <*> (pMaybe "" ("."++) (pKey "." *> pInt))

pInt' :: (IsParser p Tok) => p Int
pInt' = strToInt <$> pInt

pStr :: (IsParser p Tok) => p String
pStr = tokBlack <$> pSym (Tok TkStr "" "<string>" infpNone ScSkip)

pText :: (IsParser p Tok) => p Tok
pText = pSym (Tok TkText "" "<text>" infpNone ScSkip)

pWhiteBlack :: (IsParser p Tok) => p (Maybe String,String)
pWhiteBlack = (\t -> (mbTokWhite t,tokBlack t)) <$> pText

-------------------------------------------------------------------------
-- Shuffle chunk parser
-------------------------------------------------------------------------

type ShPr  c = (IsParser p Tok) => p c
type ShPr2 c = (IsParser p Tok) => p c -> p c
type ShPr3 c = (IsParser p Tok) => p c -> p c -> p c

mkNmForP :: String -> [String] -> Nm
mkNmForP h t = nmFromL . concat . map (wordsBy (=='.')) $ (h : t)

pAGItf :: ShPr T_AGItf
pAGItf = sem_AGItf_AGItf <$> (pFoldr (sem_Lines_Cons,sem_Lines_Nil) (sem_Line_AsIs sem_Words_Nil <$ pNl)) <*> pChunks

pAspectExprBase     ::  ShPr AspectExpr
pAspectExprBase     =   AspectExpr_Requires <$> pVar
                    <|> pParens pAspectExpr

pAspectExprAnd      ::  ShPr AspectExpr
pAspectExprAnd      =   pChainr (AspectExpr_And <$ pKey "&&") (foldr1 AspectExpr_And <$> pList1 pAspectExprBase)

pAspectExprOr       ::  ShPr AspectExpr
pAspectExprOr       =   pChainr (AspectExpr_Or <$ pKey "||") pAspectExprAnd

pAspectExpr         ::  ShPr AspectExpr
pAspectExpr         =   pAspectExprOr

pVariantRef         ::  ShPr VariantRef
pVariantRef         =   VarRef <$> pList1Sep (pKey "_") pInt'

pVariantOfferRef    ::  ShPr VariantOffer
pVariantOfferRef    =   variantOfferFromRef <$> pVariantRef

pVariantOffer       ::  ShPr VariantOffer
pVariantOffer       =   pVariantOfferRef
                    <|> pParens (VOfferRef <$> pVariantRef <*> pAspectRefsExpr)

pVariantReqmRef     ::  ShPr VariantReqm
pVariantReqmRef     =   variantReqmFromRef <$> pVariantRef

pAspectRefs         ::  ShPr AspectRefs
pAspectRefs         =   pMaybe AspectAll (AspectRefs . Set.fromList) (pList1 pVar)

pAspectRefsExpr     ::  ShPr AspectRefs
pAspectRefsExpr     =   pMaybe AspectAll AspectOfferExpr pAspectExpr

pVariantReqm        ::  ShPr VariantReqm
pVariantReqm        =   pVariantReqmRef
                    <|> pParens (VReqmRef <$> pVariantRef <*> pAspectRefs)

pOptVariantOffer    ::  ShPr VariantOffer
pOptVariantOffer    =   pMaybe VOfferAll id pVariantOffer

pVariantRefOrder    ::  ShPr VariantRefOrder
pVariantRefOrder    =   pListSep (pKey ",") (pList1Sep (pKey "<") pVariantRef)

pNm2                ::  ShPr Nm
pNm2                =   mkNmForP <$> p <*> pList (pKey "." *> (p <|> pInt))
                    where p = foldl1 (<|>) (map pKey kwTxtAsVarTooA) <|> pVar

pNm                 ::  ShPr Nm
pNm                 =   mkNmForP <$> pVar <*> pList (pKey "." *> (pVar <|> pInt))

pStrStr1            ::  ShPr String
pStrStr1            =   pVar <|> pInt <|> pStr

pStrStr2            ::  ShPr String
pStrStr2            =   pStrStr1 <|> foldl1 (<|>) (map pKey kwTxtAsVarTooB)

pStrExpr            ::  (IsParser p Tok) => p String -> p T_StrExpr -> p T_StrExpr
pStrExpr pS pPar    =   pStrExpr pStrExprBase
                    where pStrExpr     =   pChainr
                                             ((\s l r -> sem_StrExpr_Concat l (sem_StrExpr_Concat (sem_StrExpr_Str s) r)) <$> pKey ".")
                          pStrExprConc =   foldr1 sem_StrExpr_Concat <$> pList1 pStrExprBase
                          pStrExprBase =   sem_StrExpr_Str <$> pS
                                       <|> sem_StrExpr_Var <$ pKey "%" <*> pCurly pVar
                                       <|> sem_StrExpr_Group <$> pCurly (pStrExpr pStrExprConc)
                                       <|> pParens pPar

pStrExprOne         ::  (IsParser p Tok) => p String -> p T_StrExpr
pStrExprOne pS      =   pStrExprOne
                    where pStrExprOne = pStrExpr pS pStrExprOne

pStrExprSeq         ::  (IsParser p Tok) => p String -> p T_StrExprs
pStrExprSeq pS      =   pStrExprPar
                    where pStrExprWhite = foldr1 sem_StrExpr_White <$> pList1 (pStrExpr pS (sem_StrExpr_Seq <$> pStrExprPar))
                          pStrExprPar   = pFoldrSep (sem_StrExprs_Cons,sem_StrExprs_Nil) (pKey ",") pStrExprWhite

pChunkRef           ::  ShPr ChunkRef
pChunkRef           =   ChunkRef <$> pVariantRef <* pKey "." <*> pNm

pStrPacked          ::  (IsParser p Tok) => String -> String -> p a -> p a
pStrPacked o c p    =   pKey o *> p <* pKey c

pParens             ::  ShPr2 p
pParens             =   pStrPacked "(" ")"

pCurly              ::  ShPr2 p
pCurly              =   pStrPacked "{" "}"

pChunks             ::  ShPr T_Chunks
pChunks             =   pFoldr (sem_Chunks_Cons,sem_Chunks_Nil) pChunk

pChunk              ::  ShPr T_Chunk 
pChunk              =   pBegChunk
                         *> ((   sem_Chunk_Ver
                                 <$> pVariantOffer
                                 <*> pMaybe NmEmp id (pKey "." *> pNm)
                                 <*> (pKey "-" *> ((:[]) <$> pChunkRef <|> pParens (pList1 pChunkRef)) <|> pSucceed [])
                                 <*> pChunkOptions
                                 <*> pCompilerRestrictions
                                 <*> pMod
                                 <*> pImpExp "import"
                                 <*> pImpExp "export"
                             <|> sem_Chunk_Named
                                 <$> pNm
                                 <*> pMbChKind'
                             )
                             <*  pNl
                             <*> pLines
                             <*  pEndChunk
                             -- <*  pNl
                             <*> pLines
                            )
                    <?> "a chunk"
                    where pImpExp k = pKey k *> pParens (pStrExprSeq pStrStr2) <|> pSucceed sem_StrExprs_Nil
                          pMod      = sem_MbStrExpr_Just <$ pKey "module" <*> pStrExprOne pStrStr1 <|> pSucceed sem_MbStrExpr_Nothing

pCompilerRestrictions :: ShPr CompilerRestriction
pCompilerRestrictions
  = pMaybe (Restricted Nothing Nothing) id (pKey "ghc" *> pParens (Restricted <$> pCompilerVersion <* pKey "," <*> pCompilerVersion))

pCompilerVersion :: ShPr (Maybe [Int])
pCompilerVersion
  =   Just <$> pList1Sep (pKey ".") pInt'
  <|> Nothing <$ pKey "_"

pChKind             ::  ShPr ChKind
pChKind             =   pAnyFromMap pKey chKindMp

pMbChKind           ::  ShPr ChKind
pMbChKind           =   pMaybe ChAG id pChKind

pMbChKind'          ::  ShPr ChKind
pMbChKind'          =   pMaybe ChPlain id pChKind

pChDest             ::  ShPr ChDest
pChDest             =   pAnyFromMap pKey chDestMp

pMbChDest           ::  ShPr ChDest
pMbChDest           =   pMaybe ChHere id pChDest

pChWrap             ::  ShPr ChWrap
pChWrap             =   pKey "wrap" *> pKey "="
                        *> (   pAnyFromMap pKey chWrapMp
                           <|> ChWrapBeamerBlockCode <$ pKey "beamerblockcode" <*> pStr
                           <|> ChWrapBoxCode         <$ pKey "boxcode" <*> pMb (pCurly pFrac)
                           )

pMbChWrap           ::  ShPr ChWrap
pMbChWrap           =   pMaybe ChWrapPlain id pChWrap

pChunkOption        ::  ShPr T_ChunkOption
pChunkOption        =   sem_ChunkOption_Kind <$> pChKind
                    <|> sem_ChunkOption_Dest <$> pChDest
                    <|> sem_ChunkOption_Wrap <$> pChWrap

pChunkOptions       ::  ShPr T_ChunkOptions
pChunkOptions       =   pFoldr (sem_ChunkOptions_Cons,sem_ChunkOptions_Nil) pChunkOption

pLines              ::  ShPr T_Lines
pLines              =   pFoldr (sem_Lines_Cons,sem_Lines_Nil) pLine

pLine               ::  ShPr T_Line
pLine               =   sem_Line_AsIs  <$> pLineChars  <*  pNl
                    <|> (\n iv (o,r)
                          -> sem_Line_Groups 0 (sem_Groups_Cons (sem_Group_Group VOfferAll o r (sem_Lines_Cons (sem_Line_Named n iv) sem_Lines_Nil)) sem_Groups_Nil))
                             <$  pBegNameRef <*> pN <*> pMb (pKey "@" *> pVariantReqm) <*> pD <* pNl
                    <|> sem_Line_Groups 1
                             <$  pBegGroup   <*> pFoldr1Sep (sem_Groups_Cons,sem_Groups_Nil) pElseGroup pG <* pEndChunk <* pNl
                    <?> "a line"
                    where pN =   pNm
                             <|> (\v n -> mkNm v `nmApd` n) <$> pVariantRef <*> pMaybe NmEmp id (pKey "." *> pNm)
                          pD =   pChunkOptions
                             <+> pMaybe Nothing Just ((pNm2 <|> mkNm <$> pStr) <+> pMaybe Nothing Just (pKey "=" *> pMaybe "" id pStr))
                          pG =   (\v (o,r) ls -> sem_Group_Group v o r ls) <$> pOptVariantOffer <*> pD <* pNl <*> pLines

pLineChars          ::  ShPr T_Words
pLineChars          =   (foldr sem_Words_Cons sem_Words_Nil . concat)
                        <$> pList (   bwToWords1 sem_Word_Black
                                      <$> pWhiteBlack
                                  <|> bwToWords2 (\s -> sem_Word_Inline (sem_Inline_URI (tokBlack s)))
                                      <$> pBegInline <*> pText <* pEndInline
                                  <|> bwToWords2 (\s -> sem_Word_Expand s)
                                      <$> pBegExpand <*> pStrExprOne pStrStr2 <* pEndExpand
                                  )
                    where bwToWords1 sem (mw,b) =  maybe [] (\w -> [sem_Word_White w])                mw  ++ [sem b]
                          bwToWords2 sem  tk t  = (maybe [] (\w -> [sem_Word_White w]) $ mbTokWhite $ tk) ++ [sem t]

-------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------

parseHandle :: ShPr a -> FilePath -> Handle -> IO a
parseHandle p fn fh
  = do { txt <- hGetContents fh
       ; let toks = scan shuffleScanOpts ScSkip txt
       ; parseIOMessage show p toks
       }

parseAndGetRes :: ShPr a -> String -> a
parseAndGetRes p s
  = case evalSteps (parse p toks) of {Pair v _ -> v}
  where toks = scan shuffleScanOpts (ScLexMeta 0) s


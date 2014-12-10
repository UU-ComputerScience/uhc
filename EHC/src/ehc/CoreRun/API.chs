%%[(8 corerun)

-- | CoreRun Public API
--
-- Intended for constructing basic CoreRun Programs.
--
-- CoreRun is a simplified Core intended to be used for direct interpretation/execution.
-- See TBD for semantics.
--

module %%@{%{EH}%%}CoreRun.API
  (
  -- * Core AST
  -- | The datatypes making up a CoreRun program.
    Mod
  , Exp
  , SExp
  , Alt
  -- , Pat

  -- * Utilities
  , CRArray
  , mkCRArray
  
  -- * Construction functions
  , mkExp
  
  , mkInt, mkInt'
  , mkChar, mkChar'
%%[[97
  , mkInteger, mkInteger'
%%]]
  , mkString, mkString'
  , mkDbg, mkDbg'
  
  , mkApp, mkApp'
  , mkTup, mkTup'
  , mkEval
  , mkTail
  , mkCase
  , mkLam

  )
  where

import %%@{%{EH}%%}CoreRun as CR

-- **************************************
-- Construction: constants as SExp or Exp
-- **************************************

-- | Lift 'SExp' into 'Exp'
mkExp :: SExp -> Exp
mkExp = Exp_SExp

-- | Int constant as 'SExp'
mkInt' :: Int -> SExp
mkInt' = SExp_Int

-- | Int constant as 'Exp'
mkInt :: Int -> Exp
mkInt = mkExp . mkInt'

-- | Char constant as 'SExp'
mkChar' :: Char -> SExp
mkChar' = SExp_Char

-- | Char constant as 'Exp'
mkChar :: Char -> Exp
mkChar = mkExp . mkChar'

%%[[97
-- | Integer constant as 'SExp'
mkInteger' :: Integer -> SExp
mkInteger' = SExp_Integer

-- | Integer constant as 'Exp'
mkInteger :: Integer -> Exp
mkInteger = mkExp . mkInteger'
%%]]

-- | String constant as 'SExp'
mkString' :: String -> SExp
mkString' = SExp_String

-- | String constant as 'Exp'
mkString :: String -> Exp
mkString = mkExp . mkString'

-- | Debug info as 'SExp', will make an interpreter stop with displaying the message
mkDbg' :: String -> SExp
mkDbg' = dbgs

-- | Debug info as 'Exp'
mkDbg :: String -> Exp
mkDbg = dbg

-- **************************************
-- Construction: Exp
-- **************************************

-- | Application
mkApp' :: Exp -> CRArray SExp -> Exp
mkApp' = Exp_App

-- | Application
mkApp :: Exp -> [SExp] -> Exp
mkApp f as = mkApp' f (mkCRArray as)

-- | Tuple, Node
mkTup' :: Int -> CRArray SExp -> Exp
mkTup' = Exp_Tup

-- | Tuple, Node
mkTup :: Int -> [SExp] -> Exp
mkTup t as = mkTup' t (mkCRArray as)

-- | Force evaluation
mkEval :: Exp -> Exp
mkEval = Exp_Force

-- | Set tail call context
mkTail :: Exp -> Exp
mkTail = Exp_Tail

-- | Case
mkCase :: SExp -> [Exp] -> Exp
mkCase scrut alts = Exp_Case scrut $ mkCRArray $ map (Alt_Alt ref2nmEmpty) alts

-- | Lambda
mkLam
  :: Int	-- ^ nr of arguments, 0 encodes a thunk/CAF
     -> Int	-- ^ total stack size, including arguments, locals, expression calculation
     -> Exp -- ^ body
     -> Exp
mkLam nrArgs stackDepth body = Exp_Lam Nothing nrArgs stackDepth ref2nmEmpty body

%%]

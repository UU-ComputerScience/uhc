{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Interface.Basic where

import Top.Constraint
import Top.Util.Option
import Top.Monad.Select
import Top.Monad.StateFix
import Utils (internalError)

------------------------------------------------------------------------
-- (I)  Class name and (dedicated) deselect function
    
data ClassBasic = ClassBasic

deBasic :: (Embedded ClassBasic (s (StateFixT s m)) (t (StateFixT s m)), Monad m) => SelectFix t (StateFixT s m) a -> StateFixT s m a
deBasic = deselectFixFor ClassBasic

------------------------------------------------------------------------
-- (II)  Type class declaration

class Monad m => HasBasic m info | m -> info where

   -- constraints
   pushConstraint      :: Constraint m -> m ()
   pushConstraints     :: Constraints m -> m ()
   popConstraint       :: m (Maybe (Constraint m))
   discardConstraints  :: m ()
   -- errors
   addLabeledError     :: ErrorLabel -> info -> m () 
   getLabeledErrors    :: m [(info, ErrorLabel)]
   updateErrorInfo     :: (info -> m info) -> m ()
   -- conditions
   addCheck            :: String -> m Bool -> m ()
   getChecks           :: m [(m Bool, String)]
   -- options
   stopAfterFirstError :: OptionAccess m Bool
   checkConditions     :: OptionAccess m Bool

   -- defaults
   pushConstraint c    = pushConstraints [c]
   pushConstraints     = mapM_ pushConstraint
   stopAfterFirstError = ignoreOption stopOption
   checkConditions     = ignoreOption checkOption

------------------------------------------------------------------------
-- (III)  Instance for solver monad

instance ( Monad m
         , Embedded ClassBasic (s (StateFixT s m)) (t (StateFixT s m))
         , HasBasic (SelectFix t (StateFixT s m)) info
         ) => 
           HasBasic (StateFixT s m) info where
           
   -- constraints
   pushConstraint        = deBasic . pushConstraint . mapConstraint selectFix
   pushConstraints       = deBasic . pushConstraints . map (mapConstraint selectFix)
   popConstraint         = deBasic $ popConstraint >>= return . fmap (mapConstraint (deBasic))
   discardConstraints    = deBasic $ discardConstraints
   -- errors
   addLabeledError label = deBasic . addLabeledError label
   getLabeledErrors      = deBasic $ getLabeledErrors
   updateErrorInfo       = deBasic . selectFix . updateErrorInfo
   -- conditions
   addCheck s            = deBasic . addCheck s . selectFix
   getChecks             = deBasic (selectFix getChecks)
   -- options
   stopAfterFirstError   = optionAccessTrans deBasic stopAfterFirstError
   checkConditions       = optionAccessTrans deBasic checkConditions

------------------------------------------------------------------------
-- (IV)  Additional functions

pushOperation :: HasBasic m info => m () -> m ()
pushOperation = pushNamedOperation "operation"

pushNamedOperation :: HasBasic m info => String -> m () -> m ()
pushNamedOperation s = pushConstraint . operation s

addError :: HasBasic m info => info -> m ()
addError = addLabeledError NoErrorLabel

getErrors :: HasBasic m info => m [info]  
getErrors = getLabeledErrors >>= (return . map fst)

doChecks :: HasBasic m info => m ()
doChecks = 
   do ms <- getChecks
      bs <- let f x = fst x >>= (return . not)
            in filterM f ms
      unless (null bs) $ 
         let err = "\n\n  The following constraints were violated:\n" 
                   ++ unlines (map (("  - "++) . snd) bs)
         in internalError "Top.States.BasicState" "doChecks" err

startSolving  :: HasBasic m info => m ()
startSolving =
   do mc <- popConstraint
      case mc of                    
         Nothing -> 
            do check <- getOption checkConditions
               errs  <- getErrors
               when (check && null errs) doChecks
         Just c  -> 
            do solveConstraint c
               addCheck (show c) (checkCondition c)
               startSolving 

-- |A datatype to label the errors that are detected.
data ErrorLabel = ErrorLabel String 
                | NoErrorLabel 
   deriving (Eq, Ord, Show)
   
stopOption, checkOption :: Option Bool
stopOption  = option False "Stop solving constraints after the first error"
checkOption = option False "Check constraint satisfaction afterwards"
%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.AbsSynt}
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Exception})
%%]

%%[(8 codegen)


data SigTerm = Var Name 
             | Obj [(Label,Method)] 
             | Sel SigTerm Label
             | UpdM SigTerm Label Method

-- Extension lambda
             | App SigTerm SigTerm
             | Lam Name SigTerm

-- Extension aritmetica y relacional
             | Num Int
             | TT | FF
             | Binop Name SigTerm SigTerm
             | SIf SigTerm SigTerm SigTerm 
             deriving (Eq,Read, Show)

newtype Method = Met (Name, SigTerm) deriving(Eq, Read, Show)

type Name = String
type Label = Name
type Names = [Name]

type Assigns = [(Label,Method)]

updateLabelAss :: Label -> Method -> Assigns -> E Assigns
updateLabelAss l m [] = 
      errorE ("updateLabelAss: not found " ++ show m)
updateLabelAss l m ((l',m'):assgs) =
  if l == l'
  then return ((l',m):assgs)
  else do assgs' <- updateLabelAss l m assgs  -- inefficient!!
          return ((l',m'):assgs')

class Freevars a where
    freev :: a -> Names

nub :: Eq a => [a] -> [a]
nub l = nub1 l []
        where
        nub1 [] l1 = l1
        nub1 (x:xs) l1 | x `elem` l1 = nub1 xs l1
                      | otherwise = nub1 xs (x:l1)

diff :: Eq a => [a] -> [a] -> [a]
diff l1 l2 = diff1 l1 l2 []
             where
               diff1 [] l2 l = l
               diff1 (x:xs) l2 l | x `elem` l2 = diff1 xs l2 l
                                 | otherwise = diff1 xs l2 (x:l)

instance Freevars SigTerm where
    freev (Var n) = [n]
    freev (Obj lms) = nub (concatMap (freev . snd) lms)
    freev (Sel obj l) = freev obj
    freev (UpdM obj l m) = nub (freev obj ++ freev m)
    freev (App e1 e2) = nub (freev e1 ++ freev e2)
    freev (Lam n e1) = diff (freev e1) [n]
    freev (Num n) = []
    freev (TT) = []
    freev (FF) = []
    freev (Binop op e1 e2) = nub (freev e1 ++ freev e2)
    freev (SIf b e1 e2) =  nub (freev b ++ freev e1 ++ freev e2)
    
instance Freevars Method where
    freev (Met (n,obj)) = diff (freev obj) [n]

closed :: Freevars a => a -> Bool
closed = null . freev

-- For parsing

data PartialUpd = NoArrowAssg | FoundUpd Method 
                   deriving(Eq,Read,Show) 
 
assembleUpD :: SigTerm ->  PartialUpd -> SigTerm
assembleUpD e1 NoArrowAssg = e1
assembleUpD e1 (FoundUpd m) = updatem e1 m
        where updatem (Sel e l) m = UpdM e l m
              updatem _ _ = error "assembleUpD: It is not possible!!"

data PartialExpr = NoOp | FoundOp Name SigTerm
                   deriving(Eq,Read,Show) 
 
assembleOp :: SigTerm -> PartialExpr -> SigTerm
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = Binop op e1 e2 
%%]

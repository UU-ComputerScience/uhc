%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Range for location info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Range}
%%]

%%[1 import(Data.Maybe)
%%]

%%[1 import(Control.Monad)
%%]

%%[1 import(UHC.Util.Pretty)
%%]

%%[1 import(UU.Scanner.Position)
%%]

%%[1 import({%{EH}Base.HsName}, {%{EH}Base.HsName.Builtin})
%%]

%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Range
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(Range(..),emptyRange,builtinRange,mkRange1,mkRange2)
data Range
  = Range_Range    !Pos !Pos
  | Range_Unknown
  | Range_Builtin

emptyRange :: Range
emptyRange = Range_Unknown

builtinRange :: Range
builtinRange = Range_Builtin

mkPos :: Position p => p -> Pos
mkPos p = Pos (line p) (column p) (file p)

mkRange1 :: Position p => p -> Range
mkRange1 p = Range_Range (mkPos p) noPos

mkRange2 :: Position p => p -> p -> Range
mkRange2 p1 p2 = Range_Range (mkPos p1) (mkPos p2)
%%]

%%[1
show2Pos :: Pos -> Pos -> String
show2Pos p1 p2
  | p1 /= p2 && p2 /= noPos  = if line p1 == line p2
                               then mk (show (line p1))                          (Just $ show (column p1) ++ "-" ++ show (column p2))
                               else mk (show (line p1) ++ "-" ++ show (line p2)) Nothing
  | otherwise                =      mk (show (line p1))                          (Just $ show (column p1))
  where mk l c = file p1 ++ ":" ++ l ++ maybe "" (":" ++) c
%%]

%%[1
instance Show Range where
  show (Range_Range p q) = show2Pos p q
  show Range_Unknown     = "??"
  show Range_Builtin     = "builtin"

instance PP Range where
  pp = pp . show
%%]

%%[1 export(isEmptyRange)
isEmptyRange :: Range -> Bool
isEmptyRange  Range_Unknown    = True
isEmptyRange (Range_Range p _) = p == noPos
isEmptyRange  _                = False
%%]

20100209 AD: The lax equality/compare goes badly with serialization. TBD: fix this...

%%[50
instance Eq Range where
  _ == _ = True             -- a Range is ballast, not a criterium to decide equality for

instance Ord Range where
  _ `compare` _ = EQ        -- a Range is ballast, not a criterium to decide equality for
%%]

%%[1
rngAdd :: Range -> Range -> Range
rngAdd r1 r2
  = case (r1,r2) of
      (Range_Range l1 h1,Range_Range l2 h2)
        -> Range_Range (l1 `min` l2) (h1 `max` h2)
      (Range_Range _ _,_)
        -> r1
      (_,Range_Range _ _)
        -> r2
      _ -> Range_Unknown
%%]

%%[5 export(rangeUnion,rangeUnions)
posMax, posMin :: Pos -> Pos -> Pos
posMax (Pos l1 c1 f1) (Pos l2 c2 _) = Pos (l1 `max` l2) (c1 `max` c2) f1
posMin (Pos l1 c1 f1) (Pos l2 c2 _) = Pos (l1 `min` l2) (c1 `min` c2) f1

rangeUnion :: Range -> Range -> Range
rangeUnion (Range_Range b1 e1) (Range_Range b2 e2) = Range_Range (b1 `posMin` b2) (e1' `posMax` e2')
                                                  where e1' = if e1 == noPos then b1 else e1
                                                        e2' = if e2 == noPos then b2 else e2
rangeUnion Range_Unknown       r2                  = r2
rangeUnion r1                  _                   = r1

rangeUnions :: [Range] -> Range
rangeUnions = foldr1 rangeUnion
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting of Range
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.rngLift export(RngLiftArg,rngLift,rngAntilift)
type RngLiftArg  x = x
type RngLift     x = Range -> RngLiftArg x -> x

rngLift :: RngLift v
rngLift r v = v

rngAntilift :: v -> RngLiftArg v
rngAntilift = id
%%]

%%[99 -1.rngLift export(RngLiftArg,rngLift,rngAntilift)
type RngLiftArg  x = Range -> x
type RngLift     x = Range -> RngLiftArg x -> x

rngLift :: RngLift v
rngLift r mkv
  = x `seq` x
  where x = mkv r

rngAntilift :: v -> RngLiftArg v
rngAntilift = const
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Eq,Ord for Pos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
instance Eq Pos where
  p1 == p2 = line p1 == line p2 && column p1 == column p2

instance Ord Pos where
  compare p1 p2
    = case compare (line p1) (line p2) of
        EQ -> compare (column p1) (column p2)
        c  -> c
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
deriving instance Typeable Range
deriving instance Data Range

deriving instance Typeable Pos
deriving instance Data Pos

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Binary Range where
  put (Range_Unknown    ) = putWord8 0
  put (Range_Builtin    ) = putWord8 1
  put (Range_Range   a b) = putWord8 2 >> put a >> put b
  get = do t <- getWord8
           case t of
             0 -> return Range_Unknown
             1 -> return Range_Builtin
             2 -> liftM2 Range_Range get get

instance Serialize Range where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain

instance Binary Pos where
  put (Pos a b c) = put a >> put b >> put c
  get = liftM3 Pos get get get
%%]

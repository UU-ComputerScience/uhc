-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraph.Path where  

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Utils (internalError)

----------------------
   
data Path a = Path a :|: Path a   -- alternative   
            | Path a :+: Path a   -- sequence
            | Step a
            | Fail
            | Empty                   

seqList, seqList1 :: [Path a] -> Path a
seqList  = foldr  (:+:) Empty
seqList1 = foldr1 (:+:)

altList, altList1 :: [Path a] -> Path a
altList  = foldr  (:|:) Fail
altList1 = foldr1 (:|:)  

instance Show a => Show (Path a) where
   show path = 
      case path of
         x :|: y -> show x ++ "|" ++ show y
         x :+: y -> parIf (pathPrio x < 1) (show x) ++ "+" ++ parIf (pathPrio y < 1) (show y)
         Step a  -> show a
         Fail    -> "Fail"
         Empty   -> "Empty"

    where pathPrio :: Path a -> Int
          pathPrio (_ :|: _) = 0
          pathPrio (_ :+: _) = 1
          pathPrio _         = 2
          
          parIf b s = if b then "("++s++")" else s

-- |Combine two monadic computations
mCombine :: Monad m => (a -> b -> c) -> m a -> m b -> m c
mCombine op mp1 mp2 = 
   do p1 <- mp1
      p2 <- mp2
      return (p1 `op` p2)

(<+>), (<|>) :: Monad m => m (Path a) -> m (Path a) -> m (Path a)
(<+>) = mCombine (:+:)
(<|>) = mCombine (:|:)

(<++>) :: Monad m => m [Path a] -> m [Path a] -> m [Path a]
(<++>) = mCombine (++)

steps :: Path a -> [a]
steps = ($ []) . rec where
   rec path = 
      case path of 
         x :|: y -> rec x . rec y
         x :+: y -> rec x . rec y
         Step a  -> (a:)
         Fail  -> id
         Empty -> id
      
mapPath :: (a -> b) -> Path a -> Path b
mapPath f = changeStep (Step . f) 

changeStep :: (a -> Path b) -> Path a -> Path b
changeStep f = rec
 where
   rec path = 
      case path of
         Step a  -> f a
         x :|: y -> rec x :|: rec y
         x :+: y -> rec x :+: rec y
         Fail    -> Fail
         Empty   -> Empty  
      
changeStepM :: Monad m => (a -> m (Path b)) -> Path a -> m (Path b)
changeStepM f path = 
   case path of
      Step a  -> f a
      x :|: y -> do x' <- changeStepM f x; y' <- changeStepM f y; return (x' :|: y')
      x :+: y -> do x' <- changeStepM f x; y' <- changeStepM f y; return (x' :+: y')
      Fail    -> return Fail
      Empty   -> return Empty          
             
minCompleteInPath :: (a -> a -> Ordering) -> Path a -> Maybe a
minCompleteInPath f = rec . simplifyPath
   where 
      rec path = 
         case path of
            x :|: y -> do v1 <- rec x; v2 <- rec y; return (minimumBy f [v1, v2])
            x :+: y -> do v1 <- rec x; v2 <- rec y; return (maximumBy f [v1, v2])
            Step a  -> Just a
            Fail    -> Nothing
            Empty   -> Nothing

simplifyPath :: Path a -> Path a      
simplifyPath path =
   case path of  
      x :|: y -> 
         case (simplifyPath x, simplifyPath y) of
            (Empty, _    ) -> Empty
            (_    , Empty) -> Empty
            (Fail , p2   ) -> p2
            (p1   , Fail ) -> p1
            (p1   , p2   ) -> p1 :|: p2
      x :+: y -> 
         case (simplifyPath x, simplifyPath y) of
            (Fail , _    ) -> Fail
            (_    , Fail ) -> Fail    
            (Empty, p1   ) -> p1
            (p2   , Empty) -> p2
            (p1   , p2   ) -> p1 :+: p2
      _ -> path

tailSharingBy :: (a -> a -> Ordering) -> Path a -> Path a
tailSharingBy compf thePath =
   case simplifyPath thePath of 
      Empty -> Empty
      Fail  -> Fail
      p     -> rec p
      
 where
  eqf x y  = compf  x y == EQ
  eqfM x y = compfM x y == EQ
  compfM Nothing  Nothing  = EQ
  compfM (Just x) (Just y) = compf x y
  compfM m1       _        = if isJust m1 then GT else LT
  
  -- invariant: rec does not have Empty's or Fail's
  rec (Step a)    = Step a
  rec (p1 :+: p2) = p1 :+: rec p2 
  rec path =  
     let sharedTail = map (\((p, tl):rest) -> combine (p:map fst rest) tl)
                    . groupBy (\x y -> snd x  `eqfM`  snd y)
                    . sortBy  (\x y -> snd x `compfM` snd y)
                    $ [ (p, lastStep p) |  p <- altPath path ]

         combine paths Nothing   = altList1 paths
         combine paths (Just tl) = 
            case tailSharingBy compf (altList1 (map removeTail paths)) of 
               Fail  -> Fail
               Empty -> Step tl
               p     -> p :+: Step tl
            
     in altList1 sharedTail
  
  altPath :: Path a -> [Path a]
  altPath (p1 :|: p2) = altPath p1 ++ altPath p2
  altPath path        = [path]
  
  lastStep (Step a)    = Just a
  lastStep (_  :+: p2) = lastStep p2
  lastStep (p1 :|: p2) = do a <- lastStep p1
                            b <- lastStep p2
                            if a `eqf` b 
                              then Just a
                              else Nothing
  lastStep _ = internalError "Top.TypeGraph.Paths" "lastStep" "unexpected path"


  removeTail (Step _)    = Empty
  removeTail (p1 :+: p2) = p1 :+: removeTail p2
  removeTail (p1 :|: p2) = removeTail p1 :|: removeTail p2
  removeTail _           = internalError "Top.TypeGraph.Paths" "removeTail" "unexpected path"
  
flattenPath :: Path a -> [[a]]
flattenPath path = 
   case path of 
      Empty     -> [[]]
      Fail      -> []
      Step a    -> [[a]]
      p1 :+: p2 -> [ as ++ bs | as <- flattenPath p1, bs <- flattenPath p2]
      p1 :|: p2 -> flattenPath p1 ++ flattenPath p2

-- returns a list with 'smallest minimal sets'
minimalSets :: (a -> a -> Bool) -> Path a -> [[a]]
minimalSets eqF = rec where

   -- invariant: rec returns lists with the same length                
   rec path =
      case simplifyPath path of 
         Empty -> []
         Fail  -> [[]]
         p     -> 
            let a    = head (steps p)
                sol1 = rec (changeStep (\b -> if a `eqF` b then Empty else Step b) p) 
                sol2 = [ a : set
                       | set <- rec (changeStep (\b -> if a `eqF` b then Fail else Step b) p) 
                       ]
            in case (sol1, sol2) of
                  (x:_, y:_) -> 
                     case length x `compare` length y of
                        LT -> sol1
                        EQ -> sol1 ++ sol2
                        GT -> sol2
                  _ -> sol1 ++ sol2

removeSomeDuplicates :: Ord b => (a -> b) -> Path a -> Path a
removeSomeDuplicates toOrd = simplifyPath . rec M.empty where
   rec fm path = 
      case path of
      
         left :+: right ->
            case left of 
               Step a    -> let int = toOrd a
                                fm' = M.insert int Empty fm
                            in case M.lookup int fm of 
                                 Just left' -> left' :+: rec fm  right 
                                 Nothing    -> left  :+: rec fm' right
               p1 :+: p2 -> rec fm (p1 :+: (p2 :+: right))
               _         -> rec fm left :+: rec fm right
   
         left :|: right -> 
            case left of
               Step a    -> let int = toOrd a
                                fm' = M.insert int Fail fm
                            in case M.lookup int fm of 
                                  Just left' -> left' :|: rec fm  right
                                  Nothing    -> left  :|: rec fm' right
               p1 :|: p2 -> rec fm (p1 :|: (p2 :|: right))
               _         -> rec fm left :|: rec fm right
  
         Step a -> 
            M.findWithDefault path (toOrd a) fm
  
         _ -> path
 
participationMap :: Ord a => Path a -> (Integer, M.Map a Integer)
participationMap path = 
   case path of
      Empty     -> (1, M.empty)
      Fail      -> (0, M.empty)
      Step a    -> (1, M.singleton a 1)
      p1 :+: p2 -> let (i1, fm1) = participationMap p1 
                       (i2, fm2) = participationMap p2
                       fm1'      = M.map (*i2) fm1
                       fm2'      = M.map (*i1) fm2
                   in (i1 * i2, M.unionWith (\j1 j2 -> j1 + j2 - ((j1*j2) `div` (i1*i2))) fm1' fm2')
      p1 :|: p2 -> let (i1, fm1) = participationMap p1 
                       (i2, fm2) = participationMap p2
                   in (i1 + i2, M.unionWith (+) fm1 fm2)
   
pathSize :: Path a -> Int
pathSize (p1 :|: p2) = pathSize p1 + pathSize p2
pathSize (p1 :+: p2) = pathSize p1 * pathSize p2
pathSize (Step _)    = 1
pathSize _           = 0

-- |The maximal number of equality paths that is returned by equalPaths 
-- (although this number can be exceeded...it is more or less used as approximation)
-- Nothing indicates that there is no limit
maxNumberOfEqualPaths :: Maybe Int
maxNumberOfEqualPaths = Just 50

reduceNumberOfPaths :: Path a -> Path a
reduceNumberOfPaths = maybe id limitNumberOfPaths maxNumberOfEqualPaths

limitNumberOfPaths :: Int -> Path a -> Path a
limitNumberOfPaths size = fst . rec size
 where
   fromInt :: Num a => Int -> a
   fromInt = fromInteger . toInteger
   
   rec sz path = 
      case path of
         Empty     -> (path, 1)
         Fail      -> (path, 0)
         Step _    -> (path, 1)
         p1 :+: p2 -> let (p1', n1) = rec sz p1
                          newSize   
                             | n1 == 0   = sz 
                             | otherwise = ceiling ((fromInt sz / fromInt n1) :: Double)
                          (p2', n2) = rec newSize p2
                      in (p1' :+: p2', n1*n2)
         p1 :|: p2 -> let both@(p1' , n1) = rec sz p1
                          (p2', n2) = rec (sz - n1) p2
                      in if n1 >= sz
                           then both
                           else (p1' :|: p2', n1 + n2)
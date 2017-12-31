{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.State
import Control.Lens
import Data.List
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import UHC.Util.Pretty
import Text.Printf
import Debug.Trace

data Time = Time
  { _minutes :: Double
  , _seconds :: Double
  }
  deriving Show

data Timing = Timing
  { _real :: Time
  , _user :: Time
  , _sys :: Time
  }
  deriving Show

data Test = Test
  { _name :: String
  , _optimizedTimings :: [Timing]
  , _masterTimings :: [Timing]
  }
  deriving Show

data Result = Result
  { _nameR :: String
  , _number :: Int
  , _avgO :: Timing
  , _minTimeO :: Timing
  , _maxTimeO :: Timing
  , _avgM :: Timing
  , _minTimeM :: Timing
  , _maxTimeM :: Timing
  }
  deriving Show

makeLenses ''Time
makeLenses ''Timing
makeLenses ''Test
makeLenses ''Result

pTime :: String -> Parser Time
pTime s = Time <$> ((pToken s <* pSpaces) *> (pDouble <* pToken "m" <* pSpaces)) <*> pDouble <* pToken "s" <* pSpaces

pTiming :: Parser Timing
pTiming = Timing <$> pTime "real" <*> pTime "user" <*> pTime "sys"

pTest :: Parser Test
pTest = Test <$> pName <*> pTimings "Optimized" <*> pTimings "Master"

pTimings :: String -> Parser [Timing]
pTimings s = (pToken s <* pSpaces) *> pMany pTiming

pName :: Parser String
pName = pMunch (/= '*') <* pToken "*" <* pSpaces

pTests :: Parser [Test]
pTests = pMany pTest

main :: IO ()
main = do
  f <- readFile "timings"
  let ts = runParser "tests" pTests f
      res = map result ts
  mapM_ (putStrLn . show . pp) res

result :: Test -> Result
result (Test n o m) | length o == length m = Result n (length o) avgO minO maxO avgM minM maxM
  where (realsO, usersO, sysO) = timingsToTripSecs o
        (realsM, usersM, sysM) = timingsToTripSecs m
        (avgRealsO, minRealsO, maxRealsO) = getResults realsO
        (avgUsersO, minUsersO, maxUsersO) = getResults usersO
        (avgSysO, minSysO, maxSysO) = getResults sysO
        (avgRealsM, minRealsM, maxRealsM) = getResults realsM
        (avgUsersM, minUsersM, maxUsersM) = getResults usersM
        (avgSysM, minSysM, maxSysM) = getResults sysM
        avgO = Timing avgRealsO avgUsersO avgSysO
        minO = Timing minRealsO minUsersO minSysO
        maxO = Timing maxRealsO maxUsersO maxSysO
        avgM = Timing avgRealsM avgUsersM avgSysM
        minM = Timing minRealsM minUsersM minSysM
        maxM = Timing maxRealsM maxUsersM maxSysM

timingsToTripSecs :: [Timing] -> ([Double], [Double], [Double])
timingsToTripSecs = unzip3 . map timingToSecs

getResults :: [Double] -> (Time, Time, Time)
getResults xs = (secsToTime $ sum xs / genericLength xs, secsToTime $ minimum xs, secsToTime $ maximum xs)

timingToSecs :: Timing -> (Double, Double, Double)
timingToSecs t@(Timing r u s) = res
  where res = (timeToSecs r, timeToSecs u, timeToSecs s)

timeToSecs :: Time -> Double
timeToSecs (Time m s) = m * 60 + s
secsToTime :: Double -> Time
secsToTime d = Time m $ d - 60 * m
  where m = fromInteger $ toInteger $ floor (d / 60)

instance PP Result where
  pp (Result n l avgO minO maxO avgM minM maxM) = 
    n >|< ":" 
    >-< indent 2 "opt:"
    >#< "avg:" >#< avgO
    >#< "min:" >#< minO
    >#< "max:" >#< maxO
    >-< indent 2 "mas:"
    >#< "avg:" >#< avgM
    >#< "min:" >#< minM
    >#< "max:" >#< maxM
    >-< indent 2 "dif:"
    >#< "avg:" >#< per avgO avgM
    >#< "min:" >#< per minO minM
    >#< "max:" >#< per maxO maxM

per :: Timing -> Timing -> String
per (Timing r1 _ _) (Timing r2 _ _) = pert r1 r2

pert :: Time -> Time -> String
pert t1 t2 = roundToStr 1 (100 - s1 * 100 / s2) ++ "%"
  where s1 = timeToSecs t1
        s2 = timeToSecs t2

-- instance PP Timing where
--   pp (Timing r u s) = "real:" >#< r >#< "user:" >#< u >#< "sys:" >#< s

-- instance PP Time where
--   pp (Time m s) = m >|< "m" >|< s >|< "s"

instance PP Timing where
  pp (Timing r u s) = pp r

instance PP Time where
  pp = pp . timeToSecs

instance PP Double where
  pp = text . roundToStr 3

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"
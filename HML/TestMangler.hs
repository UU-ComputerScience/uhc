module TestMangler where

import TestDriver
import Control.Arrow
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.List

type Env a   = [(ID, a)]
type TestEnv = (Env Header, Env [Payload])
type Counter = IORef Int

mkHeaderEnv :: TestFile -> Env Header
mkHeaderEnv = map (hid &&& id) . heads

mkTestEnv :: TestFile -> (Int, Env [Payload])
mkTestEnv = solve . loads
  where solve :: [Payload] -> (Int, Env [Payload])
        solve []     = (0, [])
        solve (x:xs) =  case x of
                          Echo{} -> let (i, e) = solve xs
                                    in (i, (0, [x]):e)
                          Test t -> let sigs   = typeSigs t
                                        mk s x = Test $
                                                   x { typeSigs = [s]
                                                     , result   = sigResult s
                                                     }
                                        newSigs = zipWith ($) (map mk sigs) (repeat t)
                                        sigs'   = (Test $ t{typeSigs = []}) : newSigs
                                        (i, e)  = solve xs
                                        sigmax  = maximum (0:map (length . sigValue) sigs)
                                    in (i `max` ((length $ command t) + 12 + sigmax), (cmdId t, sigs'):e)
                                    
prepareEnvs :: TestFile -> (Int, TestEnv)
prepareEnvs t = (\(a,(i,b)) -> (i, (a, b))) $ (mkHeaderEnv &&& mkTestEnv) t

runTests :: (Int, TestEnv) -> (String -> String -> IO Result) -> (Int -> Counter -> Command -> Result -> IO ()) -> IO ()
runTests (width, (hd, pl)) exec notify 
 = do let count    = sum $ map (length . snd) pl
      counter  <- newIORef 0
      putStrLn $ "Preparing to run " ++ show count ++ " tests."
      execute counter hd pl pl
      succeeded <- readIORef counter
      putStrLn $ "Testing completed."
      let percent = (fromIntegral succeeded) / (fromIntegral count) * 100
      execute counter hd pl [(0,[Echo $ "SUCCEEDED: " ++ show succeeded ++ "   FAILED: " ++ show (count - succeeded) ++ "   TOTAL: " ++ show count ++ "   SUCCESS RATE: " ++ show percent])]
   where execute :: Counter -> Env Header -> Env [Payload] -> Env [Payload] -> IO ()
         execute counter henv penv []          = return ()
         execute counter henv penv ((i, x):xs) = mapM_ (run i) x >> execute counter henv penv xs
            where run _ (Echo msg) = do let banner = " " ++ replicate (width - 2) '-' ++ " "
                                        putStrLn banner
                                        putStrLn $ replicate ((width - length msg - 6) `div` 2 ) ' ' ++ "-= " ++ msg ++ " =-"
                                        putStrLn banner
                  run i (Test cmd) = do let top i = unlines $ map hvalue $ mapMaybe (flip lookup henv) i
                                            hids  = headers cmd
                                            sig   = if null (typeSigs cmd) then [] else sigValue $ head (typeSigs cmd)
                                            bod   = body cmd
                                            pays  = let ld = payloads cmd
                                                    in if null ld then [([],[[]])] else map (mixup penv) ld
                                            mk p  = case p of
                                                      (i, p') -> unlines [top (nub $ hids ++ i), concat p', sig, bod]
                                            total = map mk pays
                                            exec' = exec (command cmd)
                                        results <- mapM exec' total
                                        mapM_ (notify width counter cmd) results
                                        
mixup :: Env [Payload] -> Int -> ([ID], [String])
mixup env id = ([], [[]])
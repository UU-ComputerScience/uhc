module EH101.Base.Debug
( tr, trm, trPP )
where
import Debug.Trace
import EH.Util.Pretty

{-# LINE 18 "src/ehc/Base/Debug.chs" #-}
showPP :: PP a => a -> String
showPP p = disp (pp p) 1000 ""

trm :: String -> (a -> PP_Doc) -> a -> a
trm msg pp x = trace msg (trace (showPP (msg >|< ":" >|< pp x)) x)

tr :: String -> PP_Doc -> a -> a
tr msg p x = trm msg (const p) x

trPP :: PP a => String -> a -> a
trPP msg x = trace msg (trace (showPP (msg >|< ":" >|< x)) x)

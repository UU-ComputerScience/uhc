module Utils where

import UU.Pretty

-------------------------------------------------------------------------
-- Utils for tools
-------------------------------------------------------------------------

mkTexCmdDef :: (PP a, PP b) => String -> a -> b -> PP_Doc
mkTexCmdDef cmd nm def = "\\" >|< cmd >|< "{" >|< pp nm >|< "}{%" >-< pp def >-< "}"

mkTexCmdUse :: PP a => String -> a -> PP_Doc
mkTexCmdUse cmd nm = "\\" >|< cmd >|< "{" >|< pp nm >|< "}"

mkTexCmdUse' :: PP a => String -> a -> PP_Doc
mkTexCmdUse' cmd nm = mkTexCmdUse cmd nm >|< "%"


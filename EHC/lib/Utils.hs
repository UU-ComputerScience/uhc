module Utils where

import UU.Pretty

-------------------------------------------------------------------------
-- Utils for tools
-------------------------------------------------------------------------

mkTexCmdDef :: String -> PP_Doc -> PP_Doc -> PP_Doc
mkTexCmdDef cmd nm def = "\\" >|< cmd >|< "{" >|< nm >|< "}{%" >-< def >-< "}"

mkTexCmdUse :: String -> PP_Doc -> PP_Doc
mkTexCmdUse cmd nm = "\\" >|< cmd >|< "{" >|< nm >|< "}"

mkTexCmdUse' :: String -> PP_Doc -> PP_Doc
mkTexCmdUse' cmd nm = mkTexCmdUse cmd nm >|< "%"


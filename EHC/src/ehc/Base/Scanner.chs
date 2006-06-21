%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[5 module {%{EH}Base.Scanner}

%%]

%%[5 export(module {%{EH}Base.ScannerMachine}, module {%{EH}Base.Scanner.Token}, module {%{EH}Base.Scanner.TokenParser}, module UU.Scanner.Position)
%%]

%%[5 import({%{EH}Base.ScannerMachine}, {%{EH}Base.Scanner.Token},{%{EH}Base.Scanner.TokenParser},UU.Scanner.Position,UU.Scanner.GenTokenOrd(),UU.Scanner.GenTokenSymbol())
%%]

%%[5
instance Show Token where
  showsPrec _ token
    = showString
       (case token of
         Reserved key      pos -> "symbol "      ++ key ++ maybeshow pos
         ValToken tp val   pos -> show tp ++ " " ++ val ++ maybeshow pos
       )
instance Show EnumValToken where
 show tp = case tp of       
  TkQOp        -> "qualified operator"  
  TkQConOp     -> "qualified con operator"            
  TkOp         -> "operator"  
  TkConOp      -> "con operator"            
  TkString     -> "string"              
  TkChar       -> "character"            
  TkInteger8   -> "octal integer"         
  TkInteger10  -> "decimal Integer"       
  TkInteger16  -> "hexadecimal integer"   
  TkFraction   -> "fraction (float,...)"   
  TkQVarid     -> "lower case qualified identifier" 
  TkQConid     -> "upper case qualified identifier" 
  TkVarid      -> "lower case identifier" 
  TkConid      -> "upper case identifier" 
  TkTextnm     -> "text name"             
  TkTextln     -> "text lines"             
  TkError      -> "error in scanner:"   
  
maybeshow :: Pos -> String
maybeshow (Pos l c fn) | l <= 0 || c <= 0 =  ""
                       | otherwise        =  " at line " ++ show l
                                          ++ ", column " ++ show c
                                          ++ " of file " ++ show fn
%%]

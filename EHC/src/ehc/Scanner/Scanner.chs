%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[5 module {%{EH}Scanner.Scanner}

%%]

%%[5 export(module {%{EH}Scanner.Machine}, module {%{EH}Scanner.Token}, module {%{EH}Scanner.TokenParser}, module UU.Scanner.Position)
%%]

%%[5 import({%{EH}Scanner.Machine}, {%{EH}Scanner.Token},{%{EH}Scanner.TokenParser},UU.Scanner.Position,UU.Scanner.GenTokenOrd(),UU.Scanner.GenTokenSymbol())
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
  TkVarid            -> "lower case identifier" 
  TkConid            -> "upper case identifier" 
  TkOp               -> "operator"  
  TkConOp            -> "con operator"  
%%[[18
  TkOpUnboxed        -> "operator (unboxed)"  
  TkVaridUnboxed     -> "lower case identifier (unboxed)" 
  TkConidUnboxed     -> "upper case identifier (unboxed)" 
  TkConOpUnboxed     -> "con operator (unboxed)"            
%%]]
%%[[20
  TkQOp              -> "qualified operator"  
  TkQVarid           -> "lower case qualified identifier" 
  TkQConid           -> "upper case qualified identifier" 
  TkQConOp           -> "qualified con operator"            
%%]]
  TkString           -> "string"              
  TkChar             -> "character"            
  TkInteger8         -> "octal integer"         
  TkInteger10        -> "decimal Integer"       
  TkInteger16        -> "hexadecimal integer"   
  TkFraction         -> "fraction (float,...)"   
  TkTextnm           -> "text name"             
  TkTextln           -> "text lines"             
  TkError            -> "error in scanner:"   
  
maybeshow :: Pos -> String
maybeshow (Pos l c fn) | l <= 0 || c <= 0 =  ""
                       | otherwise        =  " at line " ++ show l
                                          ++ ", column " ++ show c
                                          ++ " of file " ++ show fn
%%]

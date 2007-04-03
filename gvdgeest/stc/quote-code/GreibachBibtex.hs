{-# OPTIONS -fglasgow-exts #-}
module Greibach where
import CPS

{----------------------------------------------------------

4.1 Bibtex

Bibtex   -> proceedings String Attrs
         |  conference  String Attrs
         
Attrs    -> title     String Attrs         
         |  year      String Attrs
         |  author    String Attrs
         |  booktitle String Attrs
         |  end
       
-----------------------------------------------------------}


data Bibtex = Proc String [(String, String)]
            --  Conf String String String String String
            
newtype Bib   a = Bib   (Bibtex             -> a)            
newtype Attrs a = Attrs ([(String, String)] -> a)            


proceedings (Bib ctx) s = lift (Attrs (\attrs -> ctx (Proc s attrs)))


title     (Attrs ctx) s = lift (Attrs (\attrs -> ctx (("title"    , s):attrs)))
year      (Attrs ctx) s = lift (Attrs (\attrs -> ctx (("year"     , s):attrs)))
author    (Attrs ctx) s = lift (Attrs (\attrs -> ctx (("author"   , s):attrs)))
booktitle (Attrs ctx) s = lift (Attrs (\attrs -> ctx (("booktitle", s):attrs)))
                                                                          
quote = lift (Bib (\s -> s))

endquote (Attrs ctx) = ctx []


instance Show Bibtex where
   show (Proc l attrs) = "@proceedings{ \"" ++ l ++ "\"\n" ++ showPairs attrs ++ "}"
                       
showPairs [] = ""
showPairs ((k, v): xs)  =  "  , " ++ k ++ " = \"" ++ v ++ "\"\n" ++ showPairs xs





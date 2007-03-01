{-| Module      :  CoreUtils
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.CodeGeneration.CoreUtils
    (   custom, customStrategy
    ,   stringToCore, coreList
    ,   let_, if_, app_, letrec_
    ,   cons, nil
    ,   var, decl
    ,   float, packedString
    ) where

import Lvm.Core.Core as Core
import Lvm.Common.Id
import Char
import Lvm.Common.Byte as Byte(bytesFromString)

infixl `app_`

custom :: String -> String -> Custom
custom sort text =
    CustomDecl
        (DeclKindCustom (idFromString sort))
        [CustomBytes (Byte.bytesFromString text)]


customStrategy :: String -> Decl a
customStrategy text =
    DeclCustom    
        { declName = idFromString ""
        , declAccess = Defined { accessPublic = True }
        , declKind = DeclKindCustom (idFromString "strategy")
        , declCustoms = [custom "strategy" text]
        }

app_ :: Expr -> Expr -> Expr
app_ f x = Ap f x

let_ :: Id -> Expr -> Expr -> Expr
let_ x e b = Let (NonRec (Bind x e)) b

letrec_ :: [CoreDecl] -> Expr -> Expr
letrec_ bs e = 
    Let 
        (Rec 
            [ Bind id expr
            | DeclValue { declName = id, valueValue = expr } <- bs
            ]
        ) 
        e

-- Function "if_" builds a Core expression of the following form
-- let! guardId = <guardExpr> in 
-- match guardId 
--   True -> <thenExpr>
--   _    -> <elseExpr>
if_ :: Expr -> Expr -> Expr -> Expr
if_ guardExpr thenExpr elseExpr =
    Let 
        (Strict (Bind guardId guardExpr))
        (Match guardId
            [ Alt (PatCon (ConId trueId) []) thenExpr
            , Alt PatDefault elseExpr
            ]
        )

-- Function "coreList" builds a linked list of the given expressions
-- Example: coreList [e1, e2] ==> 
--   Ap (Ap (Con ":") e1) 
--           (Ap (Ap (Con ":") e2)
--                    (Con "[]")
--           )
coreList :: [Expr] -> Expr
coreList = foldr cons nil

cons :: Expr -> Expr -> Expr
cons x xs = Con (ConId consId) `app_` x `app_` xs

nil :: Expr
nil = Con (ConId nilId)

( nilId : consId :  trueId :  guardId : []) = map idFromString $
  "[]"  : ":"    : "True"  : "guard$" : []

-- Function "stringToCore" converts a string to a Core expression
stringToCore :: String -> Expr
stringToCore [x] = cons (Lit (LitInt (ord x))) nil
stringToCore xs = var "$primPackedToString" `app_` packedString xs

var :: String -> Expr
var x = Var (idFromString x)

--Core.Lit (Core.LitDouble (read @value))   PUSHFLOAT nog niet geimplementeerd
float :: String -> Expr
float f = 
    Core.Ap 
        (Core.Var (idFromString "$primStringToFloat")) 
        ( Core.Lit (Core.LitBytes (bytesFromString f)) )

decl :: Bool -> String -> Expr -> CoreDecl
decl isPublic x e = 
    DeclValue 
        { declName = idFromString x
        , declAccess = Defined { accessPublic = isPublic }
        , valueEnc = Nothing
        , valueValue = e
        , declCustoms = []
        }

packedString :: String -> Expr
packedString s = Lit (LitBytes (bytesFromString s))

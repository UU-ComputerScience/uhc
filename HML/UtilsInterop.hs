-- Interop layer between the rev < 2228 and current head
module UtilsInterop (
 module UtilsInterop,
 module Utils) where
 
import EH8.EH
import EH8.Base.HsName
import EH8.Ty(tyQu_Forall)

import EqHML
 
import Utils hiding (app, appAll, apply, promote)
import qualified Utils as Utils

app :: Apply a => Sub -> a -> Maybe a
app a = return . Utils.app a

appAll :: Apply a => Env -> a -> Maybe a
appAll a = return . Utils.appAll a

apply :: Env -> Gamma -> Either Gamma [ErrorMessage]
apply e = Left . Utils.apply e

promote :: TyScheme -> Int -> TyQuantifiedScheme
promote t _ = Utils.promote t
{- |
This module is an internal helper module for 'System.CoreFoundation.String'.

We define the Template Haskell functions in this module to avoid linking problems which would
occur if they were instead defined in 'System.CoreFoundation.String'.
-}
module System.CoreFoundation.String.TH where

import Foreign.Ptr
import Foreign.C
import Foreign (peek)
import System.IO.Unsafe (unsafePerformIO)

import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH

import Prelude hiding (String)
import qualified Prelude

import Language.Haskell.TH

declareCFType "String"

{- | 
Used for importing constant strings defined in C code. For example

> importCFStringAs "kCFErrorDomainPOSIX" "domainPOSIX"

Note that a type signature may be given and documentation added:

> importCFStringAs "kCFErrorDomainPOSIX" "domainPOSIX"
> -- | Error domain for POSIX errors
> domainPOSIX :: ErrorDomain
-}
importCFStringAs :: Prelude.String -> Prelude.String -> Q [Dec]
importCFStringAs foreignStr nameStr = do
    ptrName <- newName (nameStr ++ "Ptr")
    let name = mkName nameStr
    ptrType <- [t| Ptr StringRef |]
    expr <- [| unsafePerformIO $ peek $(varE ptrName) >>= getAndRetain :: String|]
    return
        [ ForeignD $ ImportF CCall Safe ("&" ++ foreignStr) ptrName ptrType
        , FunD name [Clause [] (NormalB expr) []]
        ]


importCFString :: Prelude.String -> Q [Dec]
importCFString s = importCFStringAs s s

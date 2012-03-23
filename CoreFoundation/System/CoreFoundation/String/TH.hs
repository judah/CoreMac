{- |
This module is an internal helper module for 'System.CoreFoundation.String'.

We define the Template Haskell functions in this module to avoid linking problems which would
occur if they were instead defined in 'System.CoreFoundation.String'.
-}
module System.CoreFoundation.String.TH where

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array (allocaArray)
import Foreign (fromBool, peek)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.Data

import Prelude hiding (String)
import qualified Prelude

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Foreign (useAsPtr, fromPtr)
import Language.Haskell.TH

declareCFType "String"

importCFStringAs :: Prelude.String -> Prelude.String -> Q [Dec]
importCFStringAs foreignStr nameStr = do
    ptrName <- newName (nameStr ++ "Ptr")
    let name = mkName nameStr
    ptrType <- [t| Ptr StringRef |]
    expr <- [| unsafePerformIO $ peek $(varE ptrName) >>= getAndRetain |]
    return
        [ ForeignD $ ImportF CCall Safe ("&" ++ foreignStr) ptrName ptrType
        , SigD name (ConT ''String)
        , FunD name [Clause [] (NormalB expr) []]
        ]


importCFString :: Prelude.String -> Q [Dec]
importCFString s = importCFStringAs s s

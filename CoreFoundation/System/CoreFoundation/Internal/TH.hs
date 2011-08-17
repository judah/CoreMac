-- | Short-cuts for repeated foreign interfacing
module System.CoreFoundation.Internal.TH (
                    declareCFType
                    ) where

import System.CoreFoundation.Internal.Unsafe

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr

-- newtype CFString = CFString (ForeignPtr ())
-- instance CFType CFString where
--      unsafeCFObject = CFString
--      unsafeUnCFObject (CFString p) = p
--      getTypeID _ = _CFStringGetTypeID
-- type CFStringRef = Ptr ()
-- foreign import ccall "CFStringGetTypeID" as _CFStringGetTypeID :: IO TypeID
declareCFType :: String -> Q [Dec]
declareCFType name = do
    let n = mkName name
    p <- newName "p"
    fptr <- [t|ForeignPtr CFType|]
    ptr <- [t| CFTypeRef |]
    
    let getTypeIDStr = "CF" ++ name ++ "GetTypeID"
    getTypeIDName <- newName ("_" ++ getTypeIDStr)
    importGetTypeID <- forImpD CCall Safe getTypeIDStr getTypeIDName [t|TypeID|]

    let newtypeD = NewtypeD [] n [] (NormalC n [(NotStrict, fptr)]) []
    let inst = InstanceD [] (ConT ''CFObject `AppT` ConT n)
                [ FunD 'unsafeCFObject [Clause [] (NormalB $ ConE n) []]
                , FunD 'unsafeUnCFObject [Clause [ConP n [VarP p]]
                                    (NormalB $ VarE p) []]
                , FunD 'getTypeID [Clause [WildP] (NormalB $ VarE getTypeIDName) []]
                ]
    let tySyn = TySynD (mkName $ name ++ "Ref") [] ptr
    return [newtypeD,inst,tySyn,importGetTypeID]

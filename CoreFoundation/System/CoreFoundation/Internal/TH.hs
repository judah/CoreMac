-- | Short-cuts for repeated foreign interfacing
module System.CoreFoundation.Internal.TH (
                    declareCFType
                    ) where

import System.CoreFoundation.Internal.Unsafe

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr

-- | Generate an instance declaration for a new Core Foundation object type.
-- 
-- For example, @declareCFType "Data"@ creates a newtype called @Data@ which is
-- an instance of 'Object' and 'StaticTypeID'.

-- newtype Data = Data (ForeignPtr ())
-- instance Object Data where
--      unsafeObject = Data
--      unsafeUnObject (Data p) = p
--      maybeStaticTypeID_ = Just _CFDataGetTypeID
-- type DataRef = Ptr ()
-- foreign import ccall "CFDataGetTypeID" as _CFDataGetTypeID :: IO TypeID
-- instance StaticTypeID Data where
--      unsafeStaticTypeID _ = _CFDataGetTypeID
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
    let instObject = InstanceD [] (ConT ''Object `AppT` ConT n)
                [ FunD 'unsafeObject [Clause [] (NormalB $ ConE n) []]
                , FunD 'unsafeUnObject [Clause [ConP n [VarP p]]
                                    (NormalB $ VarE p) []]
                , FunD 'maybeStaticTypeID [Clause [WildP] (NormalB $ ConE 'Just `AppE` VarE getTypeIDName) []]
                ]
    let tySyn = TySynD (mkName $ name ++ "Ref") [] ptr
    let instStaticTypeID = InstanceD [] (ConT ''StaticTypeID`AppT` ConT n)
                            [ FunD 'unsafeStaticTypeID [Clause [WildP]
                                    (NormalB $ VarE getTypeIDName) []]
                            ]
    return [newtypeD,instObject,tySyn,importGetTypeID,instStaticTypeID]

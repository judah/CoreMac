-- | Short-cuts for repeated foreign interfacing
module System.CoreFoundation.Internal.TH (
                    declareCFType,
                    declareCFTypeAs,
                    ) where

import System.CoreFoundation.Internal.Unsafe

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr

-- | Generate an instance declaration for a new Core Foundation object type.
-- 
-- For example, @declareCFType "Data"@ creates a newtype called @Data@ which is
-- an instance of 'Object' and 'StaticTypeID'.

-- data CFData
-- newtype Data = Data (ForeignPtr CFData)
-- instance Object Data where
--      type Repr Data = CFData
--      unsafeObject = Data
--      unsafeUnObject (Data p) = p
--      maybeStaticTypeID_ = Just (TypeID _CFDataGetTypeID)
-- type DataRef = Ptr CFData
-- foreign import ccall "CFDataGetTypeID" as _CFDataGetTypeID :: CFTypeID
-- instance StaticTypeID Data where
--      unsafeStaticTypeID _ = _CFDataGetTypeID
declareCFType :: String -> Q [Dec]
declareCFType name = declareCFTypeAs ("CF" ++ name) name

declareCFTypeAs :: String -> String -> Q [Dec]
declareCFTypeAs cfname name = do
    let n = mkName name
        dn = mkName cfname
    p <- newName "p"
    fptr <- [t|ForeignPtr $(conT dn)|]
    ptr <- [t| Ptr $(conT dn) |]
    
    let getTypeIDStr = cfname ++ "GetTypeID"
    getTypeIDName <- newName ("_" ++ getTypeIDStr)
    importGetTypeID <- forImpD CCall Safe getTypeIDStr getTypeIDName [t|CFTypeID|]
    typeIDExpr <- [| TypeID $(varE getTypeIDName) |]

    let newtypeD = NewtypeD [] n [] (NormalC n [(NotStrict, fptr)]) []
    let dataD = DataD [] dn [] [] []
    let instObject = InstanceD [] (ConT ''Object `AppT` ConT n)
                [ FunD 'unsafeObject [Clause [] (NormalB $ ConE n) []]
                , FunD 'unsafeUnObject [Clause [ConP n [VarP p]]
                                    (NormalB $ VarE p) []]
                , FunD 'maybeStaticTypeID [Clause [WildP] (NormalB $ ConE 'Just `AppE` typeIDExpr) []]
                , TySynInstD ''Repr [ConT n] (ConT dn)
                ]
    let tySyn = TySynD (mkName $ name ++ "Ref") [] ptr
    let instStaticTypeID = InstanceD [] (ConT ''StaticTypeID`AppT` ConT n)
                            [ FunD 'unsafeStaticTypeID [Clause [WildP]
                                    (NormalB $ typeIDExpr) []]
                            ]
    return [dataD,newtypeD,instObject,tySyn,importGetTypeID,instStaticTypeID]



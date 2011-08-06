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
--      typeName _ = "CFString"
--      unsafeCFObject = CFString
--      unsafeUnCFObject (CFString p) = p
-- type CFStringRef = Ptr ()
declareCFType :: String -> Q [Dec]
declareCFType name = do
    let n = mkName name
    p <- newName "p"
    fptr <- [t|ForeignPtr CFType|]
    ptr <- [t| CFTypeRef |]
    let newtypeD = NewtypeD [] n [] (NormalC n [(NotStrict, fptr)]) []
    let inst = InstanceD [] (ConT ''CFObject `AppT` ConT n)
                [ FunD 'typeName [Clause [WildP]
                                    (NormalB $ LitE $ StringL name)
                                    []]
                , FunD 'unsafeCFObject [Clause [] (NormalB $ ConE n) []]
                , FunD 'unsafeUnCFObject [Clause [ConP n [VarP p]]
                                    (NormalB $ VarE p) []]
                ]
    let tySyn = TySynD (mkName $ name ++ "Ref") [] ptr
    return [newtypeD,inst,tySyn]

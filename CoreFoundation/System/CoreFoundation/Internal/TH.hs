-- | Short-cuts for repeated foreign interfacing
module System.CoreFoundation.Internal.TH (
                    unsafeForeignImport,
                    foreignExport,
                    declareCFType
                    ) where
import System.CoreFoundation.Internal.Unsafe

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr

-- "unsafe" in the sense that the foreign code should not call back
-- into Haskell.
unsafeForeignImport :: String -> TypeQ -> Q [Dec]
unsafeForeignImport name qt = do
    t <- qt
    let n = mkName $ "c_" ++ name
    return [ForeignD $ ImportF CCall Unsafe name n t]

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

foreignExport :: String -> TypeQ -> ExpQ -> Q [Dec]
foreignExport n qt qe = do
    let name = mkName n
    t <- qt
    e <- qe
    return [ ForeignD $ ExportF CCall n name t
           , SigD name t
           , FunD name [Clause [] (NormalB e) []]
           ]

    
{-
-- E.g.: foreignImportCore "CGFontCreateWithDataProvider"
--          [t| DataProviderRef -> IO Font |]
-- produces:
--  foreign import ccall unsafe "CGFontCreateWithDataProvider"
--      c_CGFontCreateWithDataProvider' :: DataProviderRef -> IO FontRef
--  c_CGFontCreateWithDataProvider :: DataProviderRef -> IO Font
--  c_CGFontCreateWithDataProvider d
--      = c_CGFontCreateWithDataProvider d >>= created
--
foreignImportCore :: String -> TypeQ -> Q [Dec]
foreignImportCore = undefined
-}

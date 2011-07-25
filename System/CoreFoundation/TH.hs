-- | Short-cuts for repeated foreign interfacing
module System.CoreFoundation.TH (
                    unsafeForeignImport,
                    declareCFType
                    ) where
import System.CoreFoundation.Base

import Language.Haskell.TH
import Foreign.Ptr
import Foreign.ForeignPtr

unsafeForeignImport :: String -> TypeQ -> Q [Dec]
unsafeForeignImport name qt = do
    t <- qt
    let n = mkName $ "c_" ++ name
    return [ForeignD $ ImportF CCall Unsafe name n t]

-- newtype CFString = CFString (ForeignPtr ())
-- instance CFType CFString where
--      typeName _ = "CFString"
--      cftype = CFString
--      uncftype (CFString p) = p
-- type CFStringRef = Ptr ()
declareCFType :: String -> Q [Dec]
declareCFType name = do
    let n = mkName name
    p <- newName "p"
    fptr <- [t|ForeignPtr ()|]
    ptr <- [t| Ptr ()|]
    let newtypeD = NewtypeD [] n [] (NormalC n [(NotStrict, fptr)]) []
    let inst = InstanceD [] (ConT ''CFType `AppT` ConT n)
                [ FunD 'typeName [Clause [WildP]
                                    (NormalB $ LitE $ StringL name)
                                    []]
                , FunD 'cftype [Clause [] (NormalB $ ConE n) []]
                , FunD 'uncftype [Clause [ConP n [VarP p]]
                                    (NormalB $ VarE p) []]
                ]
    let tySyn = TySynD (mkName $ name ++ "Ref") [] ptr
    return [newtypeD,inst,tySyn]
    
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

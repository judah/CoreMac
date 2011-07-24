-- | Short-cuts for repeated foreign interfacing
module System.CoreFoundation.TH (
                    unsafeForeignImport
                    ) where

import Language.Haskell.TH

unsafeForeignImport :: String -> TypeQ -> Q [Dec]
unsafeForeignImport name qt = do
    t <- qt
    let n = mkName $ "c_" ++ name
    return [ForeignD $ ImportF CCall Unsafe name n t]

module System.CoreFoundation.Internal.Unsafe where

import Foreign

-- | Dummy type for Core Foundation objects.
data CFType

-- | All Core Foundation API functions handle pointers to objects.  For example, 
-- the C type @CFDataRef@ is represented in Haskell by the @DataRef@ type and marshalled to the @Data@ type.
type CFTypeRef = Ptr CFType

-- | A type class for Haskell types which wrap Core Foundation objects.
class CFObject a where
    unsafeCFObject :: ForeignPtr CFType -> a
    unsafeUnCFObject :: a -> ForeignPtr CFType
    typeName :: a -> String

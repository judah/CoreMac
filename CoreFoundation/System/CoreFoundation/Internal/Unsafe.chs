module System.CoreFoundation.Internal.Unsafe where

import Foreign
import Foreign.C

#include <CoreFoundation/CoreFoundation.h>

-- | Dummy type for Core Foundation objects.
type CFType = ()

-- | A reference (i.e., pointer) to a Core Foundation object.  For example,
-- the C type @CFDataRef@ is represented in Haskell by the @DataRef@ type and marshalled to the @Data@ type.
type CFTypeRef = Ptr CFType

-- | A type class for Haskell types which wrap Core Foundation objects.
class CFObject a where
    unsafeCFObject :: ForeignPtr CFType -> a
    unsafeUnCFObject :: a -> ForeignPtr CFType
    getTypeID :: a -> TypeID

newtype TypeID = TypeID {unsafeUnTypeID :: {#type CFTypeID #}}
            deriving Eq

-- TypeIDs turn out to be safe for casting, since
-- mutable and immutable variants use the same functions, but we
-- only export the immutable API.



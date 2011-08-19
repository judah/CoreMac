module System.CoreFoundation.Internal.Unsafe where

import Foreign
import Foreign.C

#include <CoreFoundation/CoreFoundation.h>

type CFType = ()

-- | A reference (i.e., pointer) to a Core Foundation object.  For example,
-- the C type @CFDataRef@ is represented in Haskell by the @DataRef@ type and marshalled to the @Data@ type.
type CFTypeRef = Ptr CFType

-- | A type class for Haskell types which wrap Core Foundation objects.
class Object a where
    unsafeObject :: ForeignPtr CFType -> a
    unsafeUnObject :: a -> ForeignPtr CFType
    maybeStaticTypeID :: a -> Maybe TypeID -- Nothing if it's a dynamic type

newtype TypeID = TypeID {unsafeUnTypeID :: {#type CFTypeID #}}
            deriving Eq

class Object a => StaticTypeID a where
    unsafeStaticTypeID :: a -> TypeID

-- TypeIDs turn out to be safe for casting, since
-- mutable and immutable variants use the same functions, but we
-- only export the immutable API.



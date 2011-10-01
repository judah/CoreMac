module System.CoreFoundation.Internal.Unsafe where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Marshal.Array (allocaArray)
import System.IO.Unsafe (unsafePerformIO)

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

------------------------------
-- This next function, typeIDDescription,
--  is used in System.CoreFoundation.Foreign when reporting type mismatch errors,
-- so we need to define it here rather than in System.CoreFoundation.Base.

-- | Returns a textual description of the Core Foundation type identified  by the given 'TypeID'.
{#fun pure CFCopyTypeIDDescription as typeIDDescription
    { unsafeUnTypeID `TypeID' } -> `String' peekCFStringRef* #}

foreign import ccall "CFStringGetFileSystemRepresentation"
        getFileSystemRep :: CFTypeRef -> Ptr CChar -> CFIndex -> IO CBoolean

foreign import ccall "CFStringGetMaximumSizeOfFileSystemRepresentation"
        getFileSystemRepMaxSize :: CFTypeRef -> IO CFIndex

foreign import ccall "CFRelease" cfRelease :: CFTypeRef -> IO ()

peekCFStringRef :: CFTypeRef -> IO String
peekCFStringRef s = do
    len <- getFileSystemRepMaxSize s
    allocaArray (fromEnum len) $ \p -> do
    getFileSystemRep s p len
    cfRelease s
    peekCAString p

-- | This type corresponds to the C type @CFIndex@.
type CFIndex = {#type CFIndex #}

-- | This type corresponds to the C type @Boolean@.
type CBoolean = {#type Boolean #}





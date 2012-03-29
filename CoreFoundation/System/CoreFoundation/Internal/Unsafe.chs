module System.CoreFoundation.Internal.Unsafe where

import Control.Monad.Primitive
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Marshal.Array (allocaArray)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable
import Control.DeepSeq

#include <CoreFoundation/CoreFoundation.h>

data CFType

-- | A reference (i.e., pointer) to a Core Foundation object.  For example,
-- the C type @CFDataRef@ is represented in Haskell by the @DataRef@ type and marshalled to the @Data@ type.
{#pointer CFTypeRef -> CFType#}

-- | A type class for Haskell types which wrap Core Foundation objects.
class Object a where
    type Repr a
    unsafeObject :: ForeignPtr (Repr a) -> a
    unsafeUnObject :: a -> ForeignPtr (Repr a)
    maybeStaticTypeID :: a -> Maybe TypeID -- Nothing if it's a dynamic type

type CFTypeID = {#type CFTypeID#}
newtype TypeID = TypeID {unsafeUnTypeID :: CFTypeID}
            deriving (Eq, Ord, Typeable)

instance NFData TypeID

instance Show TypeID where
  show = typeIDDescription

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
        getFileSystemRep :: Ptr () -> Ptr CChar -> CFIndex -> IO CBoolean

foreign import ccall "CFStringGetMaximumSizeOfFileSystemRepresentation"
        getFileSystemRepMaxSize :: Ptr () -> IO CFIndex

foreign import ccall "CFRelease" cfRelease :: Ptr a -> IO ()

peekCFStringRef :: Ptr () -> IO String
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


------------------

-- | Some Core Foundation objects (e.g. strings and data) have mutable
-- variants.  
-- We use the Haskell types @Mutable s Data@, @Mutable s String@, etc.
-- to indicate objects which we know to be mutable.  These objects
-- can be modified in the @ST s@ monad, or the @IO@ monad (see @IOMutable@).
-- 
-- In contrast, we use the Haskell types @Data@ and @String@ to indicate objects which
-- are known to be immutable.
newtype Mutable s o = Mutable o
  deriving (Typeable, NFData)

-- | @Repr (Mutable s o) = MutableRep (Repr o)@
data MutableRepr repr

instance Object o => Object (Mutable s o) where
  type Repr (Mutable s o) = MutableRepr (Repr o)
  unsafeObject = Mutable . unsafeObject . castForeignPtr
  unsafeUnObject (Mutable o) = castForeignPtr . unsafeUnObject $ o
  maybeStaticTypeID _ = maybeStaticTypeID (undefined :: o)

-- | Mutable object for use in the 'IO' monad
type IOMutable = Mutable RealWorld

-- | Mutable object for use in the @ST s@ monad
type STMutable s = Mutable s

unsafeMutable = Mutable
unsafeUnMutable (Mutable o) = o

-- | Convert the Haskell type of a Core Foundation object to its mutable version without copying.
-- The immutable version may not be used after this operation.
unsafeThaw :: PrimMonad m => o -> m (Mutable (PrimState m) o)
unsafeThaw = return . Mutable
{-# NOINLINE unsafeThaw #-}

-- | Convert the Haskell type of a Core Foundation object to its immutable version without copying.
-- The mutable version may not be used after this operation.
unsafeFreeze :: PrimMonad m => Mutable (PrimState m) o -> m o
unsafeFreeze = return . unsafeUnMutable
{-# NOINLINE unsafeFreeze #-}

{- 
Note: NOINLINE pragmas

Consider the following use of unsafeFreeze:

  do
     mstr <- buildNewMutString
     modifyString mstr
     str <- unsafeFreeze mstr
     return (stringToText str)

We expect that the stringToText conversion occurs *after* modifyString has been run.
However, if unsafeFreeze is inlined, GHC is free to rewrite the above to

  do
     mstr <- buildNewMutString
     modifyString mstr
     let str = unsafeUnMutable mstr
     return (stringToText str)

and then to

  do 
     mstr <- buildNewMutString
     let str = unsafeUnMutable mstr
         res = stringToText str
     modifyString mstr
     return res

and now the stringToText conversion could occur *before* modifyString has run - big mistake!

To prevent GHC making such changes, we mark 'unsafeFreeze' and 'unsafeThaw' NOINLINE.
-}

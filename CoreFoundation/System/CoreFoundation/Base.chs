module System.CoreFoundation.Base(
                -- * Core Foundation Objects
                CFType,
                CFTypeRef,
                CFObject(),
                typeDescription,
                touchCF,
                withCF,
                -- ** Foreign interaction with 'CFTypeRef's
                getOwned,
                getAndRetain,
                retainCFTypeRef,
                -- * TypeIDs
                TypeID(),
                staticTypeID,
                dynamicTypeID,
                typeIDDescription,
                -- ** Casting generic objects
                Object,
                object,
                -- TODO: for generic CFObjects also
                getObjectDescription,
                castObject,
                unsafeCastObject,
                getAndRetainObject,
                withObject,
                withObjects,
                -- *  Allocators
                AllocatorRef,
                withDefaultAllocator,
                -- *  Miscellaneous type synonyms
                CBoolean,
                CFIndex,
                cvtEnum,
                ) where

import Foreign
import Foreign.C
import Control.Monad (when)

import System.CoreFoundation.Internal.Unsafe

#include <CoreFoundation/CoreFoundation.h>


-- Retains a Core Foundation object.  Returns the input.
-- If NULL, causes a crash.
foreign import ccall "CFRetain" cfRetain :: CFTypeRef -> IO CFTypeRef

-- Releases a Core Foundation object which must not be NULL.
foreign import ccall "&CFRelease" cfReleasePtr :: FunPtr (CFTypeRef -> IO ())

foreign import ccall "CFRelease" cfRelease :: CFTypeRef -> IO ()

-- | Like 'touchForeignPtr', ensures that the object will still be alive
-- at the given place in the sequence of IO events.
touchCF :: CFObject a => a -> IO ()
touchCF = touchForeignPtr . unsafeUnCFObject

-- | Like 'withForeignPtr', extracts the underlying C type.  It is not safe in general to use the 'CFTypeRef' after the action completes.
withCF :: CFObject a => a -> (CFTypeRef -> IO b) -> IO b
withCF = withForeignPtr . unsafeUnCFObject

-- | Returns a Haskell type which references the given Core Foundation C object.
-- The 'CFTypeRef' must not be null.
-- 
-- This function should be used for objects that you own, for example those obtained 
-- from a @Create@ or @Copy@ function.  
-- 
-- At some point after the Haskell type goes out of 
-- scope, the C object will be automatically released with @CFRelease@.
getOwned :: forall a . CFObject a => CFTypeRef -> IO a
getOwned p = do
    checkCFTypeRef "getOwned" p $ staticTypeID (undefined :: a)
    fmap unsafeCFObject $ newForeignPtr cfReleasePtr p

-- | Returns a Haskell type which references the given Core Foundation C object.
-- The 'CFTypeRef' must not be null.
-- 
-- This function should be used for objects that you do not own, for example 
-- those obtained from a @Get@ function.  
-- 
-- This function calls @CFRetain@ on its argument.  At
-- some point after the Haskell type goes out of 
-- scope, the C object will be automatically released with @CFRelease@.
getAndRetain :: forall a . CFObject a => CFTypeRef -> IO a
getAndRetain p = do
    checkCFTypeRef "getAndRetain" p $ staticTypeID (undefined :: a)
    cfRetain p >>= fmap unsafeCFObject . newForeignPtr cfReleasePtr

-- | Checks that the given pointer is non-null and of the right type.
-- If not, throws an error.
checkCFTypeRef :: String -> CFTypeRef -> TypeID -> IO ()
checkCFTypeRef descr p staticID
    | p==nullPtr = error $ descr ++ ": null object for type "
                            ++ show (typeIDDescription staticID)
    | otherwise = do
                    typeID <- dynamicTypeID p
                    when (typeID /= staticID)
                        $ error $ descr ++ ": type mismatch; "
                            ++ "expected " ++ show (typeIDDescription staticID)
                            ++ ", got " ++ show (typeIDDescription typeID)

{- | Returns the underlying C object, after calling an extra @CFRetain@ on it.

The C object will not be deallocated until some point after @CFRelease@ has been
called on it by foreign code (C or Objective-C).  It will also stay alive while the
Haskell type is in scope.  Every call to
'retainCFTypeRef' must be matched by exactly one call to 'CFRelease'.

Objective-C code has a few alternatives to calling 'CFRelease'; however, 
they depend on the type of memory management used by the foreign code:

  - When using manual memory management, if the 'CFTypeRef' is toll-free bridged to an
    Objective-C type then you may call @[obj autorelease]@ or @[obj release]@ instead of
    'CFRelease'.

  - When using garbage collection, you may use either 'CFMakeCollectable' or 'CFRelease'.
    You should not use @[obj release]@ or @[obj autorelease]@, as those are no-ops when
    using GC.
    
  - When using Automatic Reference Counting, if the 'CFTypeRef' is toll-free bridged 
    you may use @CFBridgingRelease@ instead of @CFRelease@ to indicate that ARC will be
    responsible for releasing the object. (Untested.)
-}
retainCFTypeRef :: CFObject a => a -> IO CFTypeRef
retainCFTypeRef x = withCF x cfRetain

----------

type AllocatorRef = Ptr ()

withDefaultAllocator :: (AllocatorRef -> IO a) -> IO a
withDefaultAllocator f = f nullPtr


--------

-- | Returns the 'TypeID' for objects of type @a@.  Does not use the argument.
staticTypeID :: CFObject a => a -> TypeID
staticTypeID = getTypeID

-- | Examines the given 'CFTypeRef' to determine its type.
{#fun CFGetTypeID as dynamicTypeID
    { id `CFTypeRef' } -> `TypeID' TypeID #}





cvtEnum :: (Enum a, Enum b) => a -> b
cvtEnum = toEnum . fromEnum

-------
-- Misc types

-- | This type corresponds to the C type @CFIndex@.
type CFIndex = {#type CFIndex #}

-- | This type corresponds to the C type @Boolean@.
type CBoolean = {#type Boolean #}

------
-- Getting the String from a TypeID.
-- The CF.String module provides a better API, but using it would lead to cyclic imports.

foreign import ccall "CFStringGetFileSystemRepresentation"
        getFileSystemRep :: CFTypeRef -> Ptr CChar -> CFIndex -> IO CBoolean

foreign import ccall "CFStringGetMaximumSizeOfFileSystemRepresentation"
        getFileSystemRepMaxSize :: CFTypeRef -> IO CFIndex

-- | Returns a textual description of the Core Foundation type identified  by the given 'TypeID'.
{#fun pure CFCopyTypeIDDescription as typeIDDescription
    { unsafeUnTypeID `TypeID' } -> `String' peekCFStringRef* #}

peekCFStringRef :: CFTypeRef -> IO String
peekCFStringRef s = do
    len <- getFileSystemRepMaxSize s
    allocaArray (fromEnum len) $ \p -> do
    getFileSystemRep s p len
    cfRelease s
    peekCAString p


-- | Returns a textual description of the Core Foundation type associated with the Haskell type @a@.
typeDescription :: CFObject a => a -> String
typeDescription = typeIDDescription . staticTypeID

newtype Object = Object (ForeignPtr CFType)

object :: CFObject a => a -> Object
object = Object . unsafeUnCFObject

{#fun CFCopyDescription as getObjectDescription
    { withObject* `Object' } -> `String' peekCFStringRef* #}

castObject :: forall a . CFObject a => Object -> Maybe a
castObject (Object o) = unsafePerformIO $ do
                            t <- withForeignPtr o dynamicTypeID
                            return $ if staticTypeID (undefined :: a) == t
                                then Just $ unsafeCFObject o
                                else Nothing

-- | Throws an error if the input is not of the given type.
unsafeCastObject :: forall a . CFObject a => Object -> a
unsafeCastObject o = case castObject o of
                        Just x -> x
                        Nothing -> error $ "unsafeCastObject: expected type "
                                    ++ show (typeDescription (undefined :: a))

getAndRetainObject :: CFTypeRef -> IO Object
getAndRetainObject p
    | p==nullPtr = error "getAndRetainObject: null object"
    | otherwise = cfRetain p >>= fmap Object . newForeignPtr cfReleasePtr

withObject :: Object -> (CFTypeRef -> IO a) -> IO a
withObject (Object o) = withForeignPtr o

-- TODO: is this inefficient?
withObjects :: [Object] -> ([CFTypeRef] -> IO b) -> IO b
withObjects [] act = act []
withObjects (o:os) act = withObject o $ \p -> withObjects os $ \ps -> act (p:ps)

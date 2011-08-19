module System.CoreFoundation.Base(
                -- * Core Foundation Objects
                CFType,
                CFTypeRef,
                Object(),
                touchObject,
                withObject,
                withObjects,
                -- ** Foreign interaction with 'CFTypeRef's
                getOwned,
                getAndRetain,
                retainCFTypeRef,
                -- * TypeIDs
                TypeID(),
                typeIDDescription,
                StaticTypeID,
                staticTypeID,
                dynamicTypeID,
                staticTypeDescription,
                dynamicTypeDescription,
                -- ** Dynamic types
                DynObj,
                dyn,
                castObject,
                castObjectOrError,
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
touchObject :: Object a => a -> IO ()
touchObject = touchForeignPtr . unsafeUnObject

-- | Like 'withForeignPtr', extracts the underlying C type and keeps the object alive
-- while the given action is running.
-- It is not safe in general to use the 'CFTypeRef' after the action completes.
withObject :: Object a => a -> (CFTypeRef -> IO b) -> IO b
withObject = withForeignPtr . unsafeUnObject

-- | Returns a Haskell type which references the given Core Foundation C object.
-- The 'CFTypeRef' must not be null.
-- 
-- This function should be used for objects that you own, for example those obtained 
-- from a @Create@ or @Copy@ function.  
-- 
-- At some point after the Haskell type goes out of 
-- scope, the C object will be automatically released with @CFRelease@.
getOwned :: forall a . Object a => CFTypeRef -> IO a
getOwned p = do
    checkCFTypeRef "getOwned" p $ maybeStaticTypeID (undefined :: a)
    fmap unsafeObject $ newForeignPtr cfReleasePtr p

-- | Returns a Haskell type which references the given Core Foundation C object.
-- The 'CFTypeRef' must not be null.
-- 
-- This function should be used for objects that you do not own, for example 
-- those obtained from a @Get@ function.  
-- 
-- This function calls @CFRetain@ on its argument.  At
-- some point after the Haskell type goes out of 
-- scope, the C object will be automatically released with @CFRelease@.
getAndRetain :: forall a . Object a => CFTypeRef -> IO a
getAndRetain p = do
    checkCFTypeRef "getAndRetain" p $ maybeStaticTypeID (undefined :: a)
    cfRetain p >>= fmap unsafeObject . newForeignPtr cfReleasePtr

-- | Checks that the given pointer is non-null and of the right type.
-- If not, throws an error.
checkCFTypeRef :: String -> CFTypeRef -> Maybe TypeID -> IO ()
checkCFTypeRef descr p maybeStaticID
    | p==nullPtr = error $ descr ++ ": null object"
    | otherwise = case maybeStaticID of
                    Nothing -> return ()
                    Just staticID -> do
                        dynTypeID <- cfTypeID p
                        when (dynTypeID /= staticID)
                            $ error $ descr ++ ": type mismatch; "
                                ++ "expected " ++ show (typeIDDescription staticID)
                                ++ ", got " ++ show (typeIDDescription dynTypeID)

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
retainCFTypeRef :: Object a => a -> IO CFTypeRef
retainCFTypeRef x = withObject x cfRetain

----------

type AllocatorRef = Ptr ()

withDefaultAllocator :: (AllocatorRef -> IO a) -> IO a
withDefaultAllocator f = f nullPtr


--------

-- | Examines the given 'CFTypeRef' to determine its type.
{#fun pure CFGetTypeID as dynamicTypeID
    `Object a' => { withObject* `a' } -> `TypeID' TypeID #}

-- | Returns the 'TypeID' associated with objects of type @a@.  
-- Does not use its argument.
staticTypeID :: StaticTypeID a => a -> TypeID
staticTypeID = unsafeStaticTypeID

-- Helper
{#fun CFGetTypeID as cfTypeID
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
staticTypeDescription :: StaticTypeID a => a -> String
staticTypeDescription = typeIDDescription . staticTypeID

-- | Returns a textual description of the type of the given Core Foundation object.
dynamicTypeDescription :: Object a => a -> String
dynamicTypeDescription = typeIDDescription . dynamicTypeID

-- | A 'DynObj' wraps a Core Foundation object of unknown type.
newtype DynObj = DynObj (ForeignPtr CFType)

instance Object DynObj where
    unsafeObject = DynObj
    unsafeUnObject (DynObj o) = o
    maybeStaticTypeID _ = Nothing

dyn :: Object a => a -> DynObj
dyn = DynObj . unsafeUnObject

castObject  :: forall a . Object a => DynObj -> Maybe a
castObject o@(DynObj p) = case maybeStaticTypeID (undefined :: a) of
                    Just t | t /= dynamicTypeID o   -> Nothing
                    _                               -> Just $ unsafeObject p

{#fun CFCopyDescription as getObjectDescription
    `Object a' => { withObject* `a' } -> `String' peekCFStringRef* #}

-- | Throws an error if the input is not of the given type.
castObjectOrError :: forall a . Object a => DynObj -> a
castObjectOrError o@(DynObj p)
    = case maybeStaticTypeID (undefined :: a) of
        Just t | t /= dynamicTypeID o   -> error $ "unsafeCastObject: expected type "
                                            ++ show (typeIDDescription t)
        _ -> unsafeObject p

-- TODO: is this inefficient?
withObjects :: Object a => [a] -> ([CFTypeRef] -> IO b) -> IO b
withObjects [] act = act []
withObjects (o:os) act = withObject o $ \p -> withObjects os $ \ps -> act (p:ps)

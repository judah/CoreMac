module System.CoreFoundation.Foreign(
                -- * Foreign interaction with 'CFTypeRef's
                -- $foreign
                -- Marshalling Core Foundation objects to/from Haskell
                CFType,
                CFTypeRef,
                withObject,
                withMaybeObject,
                withObjects,
                getOwned,
                getAndRetain,
                maybeGetOwned,
                maybeGetAndRetain,
                retainCFTypeRef,
                -- * Mutable types
                unsafeMutable,
                -- *  Allocators
                AllocatorRef,
                withDefaultAllocator,

                ) where

import System.CoreFoundation.Internal.Unsafe

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal (allocaArray)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)

#include <CoreFoundation/CoreFoundation.h>

----------
-- Foreign retain/release functions

-- Retains a Core Foundation object.  Returns the input.
-- If NULL, causes a crash.
foreign import ccall "CFRetain" cfRetain :: CFTypeRef -> IO CFTypeRef

-- Releases a Core Foundation object which must not be NULL.
foreign import ccall "&CFRelease" cfReleasePtr :: FunPtr (CFTypeRef -> IO ())

------------------------------


-- Helper
{#fun CFGetTypeID as cfTypeID
    { id `CFTypeRef' } -> `TypeID' TypeID #}

{- $foreign

We note one caveat about the foreign export functions.   Namely, the pure
object constructors like @String.fromChars@ and @Number.newNumber@ break
referential transparency if the underlying 'CFTypeRef's are tested for equality.  For
example:

> -- Returns False
> test1 = let
>           str1 = fromChars "foo"
>           str2 = fromChars "foo"
>         in withObject str1 $ \p1 -> withObject str2 $ \p2 -> return p1==p2
> 
> -- Returns True
> test1 = let
>           str = fromChars "foo"
>         in withObject str $ \p1 -> withObject str $ \p2 -> return p1==p2

In general, however, this should not cause problems when using the 
Core Foundation API functions.

-}

-- | Like 'withForeignPtr', extracts the underlying C type and keeps the object alive
-- while the given action is running.
-- It is not safe in general to use the 'CFTypeRef' after the action completes.
withObject :: Object a => a -> (CFTypeRef -> IO b) -> IO b
withObject = withForeignPtr . unsafeUnObject

-- | Like 'withObject', except that if the input is Nothing, the action will be passed a 'nullPtr'.
withMaybeObject :: Object a => Maybe a -> (CFTypeRef -> IO b) -> IO b
withMaybeObject Nothing = ($ nullPtr)
withMaybeObject (Just o) = withObject o

-- TODO: is this inefficient?
withObjects :: Object a => [a] -> ([CFTypeRef] -> IO b) -> IO b
withObjects [] act = act []
withObjects (o:os) act = withObject o $ \p -> withObjects os $ \ps -> act (p:ps)

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

-- | Retuns a Haskell type which references the given Core Foundation C object.
-- This function performs the same as 'getOwned', except that it returns 'Nothing'
-- if the input is NULL.
maybeGetOwned :: Object a => CFTypeRef -> IO (Maybe a)
maybeGetOwned p
    | p==nullPtr = return Nothing
    | otherwise = fmap Just $ getOwned p

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

-- | Retuns a Haskell type which references the given Core Foundation C object.
-- This function performs the same as 'getAndRetain', except that it returns 'Nothing'
-- if the input is NULL.
maybeGetAndRetain :: Object a => CFTypeRef -> IO (Maybe a)
maybeGetAndRetain p
    | p==nullPtr = return Nothing
    | otherwise = fmap Just $ getAndRetain p

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

--------



----------

type AllocatorRef = Ptr ()

withDefaultAllocator :: (AllocatorRef -> IO a) -> IO a
withDefaultAllocator f = f nullPtr




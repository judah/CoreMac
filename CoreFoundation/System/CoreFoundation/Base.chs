module System.CoreFoundation.Base(
                -- * Core Foundation Objects
                CFType,
                CFTypeRef,
                CFObject(),
                touchCF,
                withCF,
                -- ** Foreign interaction with 'CFTypeRef's
                getOwned,
                getAndRetain,
                retainCFTypeRef,
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
getOwned p
    | p==nullPtr = error $ "getOwned: null object of type "
                            ++ typeName (undefined :: a)
    | otherwise = fmap unsafeCFObject $ newForeignPtr cfReleasePtr p

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
getAndRetain p
    | p==nullPtr = error $ "getAndRetain: null object of type "
                            ++ typeName (undefined :: a)
    | otherwise = cfRetain p >>= fmap unsafeCFObject . newForeignPtr cfReleasePtr


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

-------
-- Misc types

-- | This type corresponds to the C type @Boolean@.
type CBoolean = {#type Boolean #}

-- | This type corresponds to the C type @CFIndex@.
type CFIndex = {#type CFIndex #}

cvtEnum :: (Enum a, Enum b) => a -> b
cvtEnum = toEnum . fromEnum

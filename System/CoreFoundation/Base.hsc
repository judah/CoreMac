-- CFBase.h header
-- We'll use the Haskell GC to keep objects alive until Haskell-land is
-- done with them.  (CFRetain/CFRelease keep count even if Objective C is GC'd;
-- the only difference is that the object may not be immediately released after its
-- retain count drops to zero.)
--
-- In general, we'll be using the default allocator.
module System.CoreFoundation.Base where


import Foreign

#include <ApplicationServices/ApplicationServices.h>

type CFTypeRef = Ptr ()

-- Retains a Core Foundation object.  Returns the input.
-- If NULL, causes a crash.
foreign import ccall "CFRetain" cfRetain :: CFTypeRef -> IO CFTypeRef

-- Releases a Core Foundation object which must not be NULL.
foreign import ccall "&CFRelease" cfReleasePtr :: FunPtr (CFTypeRef -> IO ())

foreign import ccall "CFRelease" cfRelease :: CFTypeRef -> IO ()

foreign import ccall "CFEqual" cfEqual :: CFTypeRef -> CFTypeRef -> IO Bool
-- Also hash and copyDescription and so on.
--

-- Unsafe to access these directly.
class CFType a where
    cftype :: ForeignPtr () -> a
    uncftype :: a -> ForeignPtr ()
    typeName :: a -> String

-- | For return values which we own (Create, Copy)
created :: forall a . CFType a => CFTypeRef -> IO a
created p
    -- CFRetain, CFRelease require non-null arguments.
    | p == nullPtr = error $ "created: null object of type " ++ typeName (undefined :: a)
    | otherwise = fmap cftype $ newForeignPtr cfReleasePtr p

-- | For return values which we don't own (e.g., Get) 
-- We retain them in the Haskell GC.
retain :: forall a . CFType a => CFTypeRef -> IO a
retain p
    -- CFRetain, CFRelease require non-null arguments.
    | p == nullPtr = error $ "retain: null object of type " ++ typeName (undefined :: a)
    | otherwise = cfRetain p >>= fmap cftype . newForeignPtr cfReleasePtr

withCF :: CFType a => a -> (Ptr () -> IO b) -> IO b
withCF = withForeignPtr . uncftype

----------

newtype CFAllocatorRef = CFAllocatorRef (Ptr ())

defaultAllocatorRef :: CFAllocatorRef
defaultAllocatorRef = CFAllocatorRef nullPtr -- or the other one

unAllocatorRef :: CFAllocatorRef -> Ptr ()
unAllocatorRef (CFAllocatorRef p) = p

-------
-- Misc types
type CBool = #type bool
type CBoolean = #type Boolean

type CFIndex = #type CFIndex

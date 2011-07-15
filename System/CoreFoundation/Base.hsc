-- CFBase.h header
-- We'll use the Haskell GC to keep objects alive until Haskell-land is
-- done with them.  (CFRetain/CFRelease keep count even if Objective C is GC'd;
-- the only difference is that the object may not be immediately released after its
-- retain count drops to zero.)
--
-- In general, we'll be using the default allocator.
module System.CoreFoundation.Base where


import Foreign

#include <stdbool.h>

type CFTypeRef = Ptr ()

-- Retains a Core Foundation object.  Returns the input.
-- If NULL, causes a crash.
foreign import ccall "CFRetain" cfRetain :: CFTypeRef -> IO CFTypeRef

-- Releases a Core Foundation object which must not be NULL.
foreign import ccall "&CFRelease" cfRelease :: FunPtr (CFTypeRef -> IO ())

foreign import ccall "CFEqual" cfEqual :: CFTypeRef -> CFTypeRef -> IO Bool
-- Also hash and copyDescription and so on.
--

class CFType a where
    cftype :: ForeignPtr () -> a
    uncftype :: a -> ForeignPtr ()

retained :: CFType a => Ptr () -> IO a
retained p = cfRetain p >>= fmap cftype . newForeignPtr cfRelease

cfWith :: CFType a => a -> (Ptr () -> IO b) -> IO b
cfWith = withForeignPtr . uncftype


newtype CFAllocatorRef = CFAllocatorRef (Ptr ())

defaultAllocatorRef :: CFAllocatorRef
defaultAllocatorRef = CFAllocatorRef nullPtr -- or the other one

unAllocatorRef :: CFAllocatorRef -> Ptr ()
unAllocatorRef (CFAllocatorRef p) = p

-------
-- Misc types
type CBool = #type bool

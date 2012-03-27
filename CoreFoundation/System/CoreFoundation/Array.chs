-- | Core Foundation Arrays.  These are toll-free bridged with 'NSArray'.
module System.CoreFoundation.Array(
                    Array,
                    ArrayRef,
                    CFArray,
                    -- * Accessing elements
                    getCount,
                    getObjectAtIndex,
                    -- * Creating arrays
                    fromList,
                    ) where


import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal (withArrayLen)
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative((<$>))
import Control.Monad(when)
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH

-- | The opaque CoreFoundation @CFArray@ type.
data CFArray
{- | 
Arrays of pointers. Wraps the @CFArrayRef@ type.
-}
newtype Array a = Array { unArray :: ForeignPtr CFArray }
{#pointer CFArrayRef as ArrayRef -> CFArray#}

instance Object (Array a) where
  type Repr (Array a) = CFArray
  unsafeObject = Array
  unsafeUnObject = unArray
  maybeStaticTypeID _ = Just _CFArrayGetTypeID

foreign import ccall "CFArrayGetTypeID" _CFArrayGetTypeID :: TypeID
instance StaticTypeID (Array a) where
  unsafeStaticTypeID _ = _CFArrayGetTypeID

#include <CoreFoundation/CoreFoundation.h>

-- | Returns the number of values currently stored in an array.
{#fun pure unsafe CFArrayGetCount as getCount
    { withObject* `Array a' } -> `Int' #}

{#fun pure unsafe CFArrayGetValueAtIndex as getPtrAtIndex
    `Object a' => { withObject* `Array a', `Int' } -> `a' '(getAndRetain . castPtr)'* #}

-- TODO: out-of-bounds errors will cause this to crash!
-- Also, note that this fails if the object isn't of the right type.

-- | Returns the object stored at the given index of the array.
-- 
-- This function throws an error if the index is out of bounds, 
-- or if the object cannot be casted to type @a@.
getObjectAtIndex :: Object a => Array a -> Int -> a
getObjectAtIndex a k
 | k < 0 || k >= n = error $ "getObjectAtIndex: out-of-bounds: " ++ show (k,n)
 | otherwise = getPtrAtIndex a k
  where n = getCount a

-- For when all the elements are CFType-derived.
foreign import ccall "&" kCFTypeArrayCallBacks :: Ptr ()

{#fun CFArrayCreate as cfArrayCreate
    { withDefaultAllocator- `AllocatorPtr'
    , id `Ptr (Ptr ())'
    , `Int'
    , id `Ptr ()'
    } -> `ArrayRef' id #}

-- | Returns a new immutable 'Array' which contains the elements of the given list.
fromList :: Object a => [a] -> Array a
fromList objs = unsafePerformIO $ withObjects objs $ \ps ->
                    withArrayLen ps $ \ n p ->
                        getOwned $ cfArrayCreate (castPtr p) n kCFTypeArrayCallBacks
                        


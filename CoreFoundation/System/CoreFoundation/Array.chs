-- | Core Foundation Arrays.  These are toll-free bridged with 'NSArray'.
module System.CoreFoundation.Array(
                    Array,
                    ArrayRef,
                    -- * Accessing elements
                    getCount,
                    getObjectAtIndex,
                    -- * Creating arrays
                    fromList,
                    ) where


import Foreign.C
import Foreign.Ptr
import Foreign.Marshal (withArrayLen)
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative((<$>))
import Control.Monad(when)
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH

declareCFType "Array"
{#pointer CFArrayRef as ArrayRef nocode #}

#include <CoreFoundation/CoreFoundation.h>

-- | Returns the number of values currently stored in an array.
{#fun unsafe CFArrayGetCount as getCount
    { withObject* `Array' } -> `Int' #}

{#fun unsafe CFArrayGetValueAtIndex as getPtrAtIndex
    { withObject* `Array', `Int' } -> `Ptr ()' id #}

-- TODO: out-of-bounds errors will cause this to crash!
-- Also, note that this fails if the object isn't of the right type.

-- | Returns the object stored at the given index of the array.
-- 
-- This function throws an error if the index is out of bounds, 
-- or if the object cannot be casted to type @a@.
getObjectAtIndex :: Object a => Array -> Int -> IO a
getObjectAtIndex a k = do
    n <- getCount a
    when (k < 0 || k >= n) $ error $ "getObjectAtIndex: out-of-bounds: " ++ show (k,n)
    castObjectOrError <$> (getPtrAtIndex a k >>= getAndRetain)

-- For when all the elements are CFType-derived.
foreign import ccall "&" kCFTypeArrayCallBacks :: Ptr ()

{#fun CFArrayCreate as cfArrayCreate
    { withDefaultAllocator- `AllocatorPtr'
    , id `Ptr (Ptr ())'
    , `Int'
    , id `Ptr ()'
    } -> `Array' getOwned* #}

-- | Returns a new immutable 'Array' which contains the elements of the given list.
fromList :: Object a => [a] -> Array
fromList objs = unsafePerformIO $ withObjects (map dyn objs) $ \ps ->
                    withArrayLen ps $ \ n p ->
                        cfArrayCreate p n kCFTypeArrayCallBacks
                        


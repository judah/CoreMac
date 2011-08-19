-- | Core Foundation Arrays.  These are toll-free bridged with 'NSArray'.
module System.CoreFoundation.Array(
                    Array,
                    ArrayRef,
                    -- * Accessing elements
                    getCount,
                    getObjectAtIndex,
                    -- * Creating arrays
                    newArray,
                    ) where


import Foreign hiding (newArray)
import Foreign.C
import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

declareCFType "Array"

#include <CoreFoundation/CoreFoundation.h>

-- | Returns the number of values currently stored in an array.
{#fun unsafe CFArrayGetCount as getCount
    { withObject* `Array' } -> `Int' #}

{#fun unsafe CFArrayGetValueAtIndex as getPtrAtIndex
    { withObject* `Array', `Int' } -> `Ptr ()' id #}

-- TODO: out-of-bounds errors will cause this to crash!
-- Also, note that this fails if the object isn't of the right type.

getObjectAtIndex :: Object a => Array -> Int -> IO a
getObjectAtIndex a k = getPtrAtIndex a k >>= getAndRetain

-- For when all the elements are CFType-derived.
foreign import ccall "&" kCFTypeArrayCallBacks :: Ptr ()

{#fun CFArrayCreate as cfArrayCreate
    { withDefaultAllocator- `AllocatorPtr'
    , id `Ptr (Ptr ())'
    , `Int'
    , id `Ptr ()'
    } -> `Array' getOwned* #}

newArray :: Object a => [a] -> IO Array
newArray objs = withObjects objs $ \ps ->
                    withArrayLen ps $ \ n p ->
                        cfArrayCreate p n kCFTypeArrayCallBacks
                        


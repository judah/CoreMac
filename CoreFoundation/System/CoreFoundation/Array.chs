-- | Core Foundation Arrays.  These are toll-free bridged with 'NSArray'.
module System.CoreFoundation.Array(
                    Array,
                    ArrayRef,
                    -- * Accessing elements
                    getCount,
                    getObjectAtIndex,
                    -- * Creating arrays
                    newArrayFromList,
                    ) where


import Foreign
import Foreign.C
import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

declareCFType "Array"

#include <CoreFoundation/CoreFoundation.h>

-- | Returns the number of values currently stored in an array.
{#fun unsafe CFArrayGetCount as getCount
    { withCF* `Array' } -> `Int' #}

{#fun unsafe CFArrayGetValueAtIndex as getPtrAtIndex
    { withCF* `Array', `Int' } -> `Ptr ()' id #}

getObjectAtIndex :: Array -> Int -> IO Object
getObjectAtIndex a k = getPtrAtIndex a k >>= getAndRetainObject

-- For when all the elements are CFType-derived.
foreign import ccall "&" kCFTypeArrayCallBacks :: Ptr ()

{#fun CFArrayCreate as cfArrayCreate
    { withDefaultAllocator- `AllocatorPtr'
    , id `Ptr (Ptr ())'
    , `Int'
    , id `Ptr ()'
    } -> `Array' getOwned* #}

newArrayFromList :: [Object] -> IO Array
newArrayFromList objs = withObjects objs $ \ps ->
                    withArrayLen ps $ \ n p ->
                        cfArrayCreate p n kCFTypeArrayCallBacks
                        


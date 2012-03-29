-- | Core Foundation mutable arrays.  These are toll-free bridged with
-- `NSMutableArray'.
module System.CoreFoundation.Array.Mutable(
                            MArray,
                            newMutableArray,
                            appendValue,
                            setValueAtIndex,
                            insertValueAtIndex,
                            ) where

import Foreign.Ptr
import Foreign.C
import Control.Monad (when)
import Control.Monad.Primitive

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
{#import System.CoreFoundation.Array#} (Array, CFArray, ArrayRef)

#include <CoreFoundation/CoreFoundation.h>

-- | Mutable arrays
type MArray s a = Mutable s (Array a)

type MArrayRef = Ptr (MutableRepr CFArray)
{#pointer CFMutableArrayRef as MArrayRef nocode #}
foreign import ccall "&" kCFTypeArrayCallBacks :: Ptr ()

-- | Create a new mutable array.  The array starts empty and can contain up to the
-- given number of values.
{#fun CFArrayCreateMutable as cfNewMutableArray
    { withDefaultAllocator- `AllocatorPtr'
    , `Int'
    , '($ kCFTypeArrayCallBacks)'- `Ptr ()'
    } -> `MArrayRef' id#}

-- | Returns the number of values currently stored in an array.
getCount :: PrimMonad m => MArray (PrimState m) a -> m Int
getCount arr = unsafePrimToPrim $ c_getCount arr

{#fun unsafe CFArrayGetCount as c_getCount
    { withMutableObject* `Mutable s (Array a)' } -> `Int' #}

{- | 
Construct an empty array with the given capacity.
    
  [capacity] The maximum number of values that can be contained by the new array. The array starts empty and can grow to this number of values (and it can have less). Pass 0 to specify that the maximum capacity is not limited. The value must not be negative.
-}
newMutableArray :: (PrimMonad m, Object a) => Int -> m (MArray (PrimState m) a)
newMutableArray n = unsafePrimToPrim . getOwned $ cfNewMutableArray n

{- |
Adds a value to an array giving it the new largest index. If the array is a limited-capacity array and it is full before this operation, the behavior is undefined.

  [Discussion] The value parameter is assigned to the index one larger than the previous largest index and the count of the Array is increased by one.
-}
appendValue :: (PrimMonad m, Object a) => MArray (PrimState m) a -> a -> m ()
appendValue arr v  = unsafePrimToPrim $ c_appendValue arr v

{#fun CFArrayAppendValue as c_appendValue
    `Object o' => { withObject* `Mutable s (Array o)'
    , withVoidObject* `o'
    } -> `()' #}

{#fun CFArraySetValueAtIndex as c_setValueAtIndex
    `Object o' =>
    { withObject* `Mutable s (Array o)'
    , `Int'
    , withVoidObject* `o'
    } -> `()' #}

-- | Change the value at the given index in the array.
--
-- The index must be less than the current size of the array.
setValueAtIndex :: (PrimMonad m, Object o) => MArray (PrimState m) o -> Int -> o -> m ()
setValueAtIndex a i x = do
    n <- getCount a
    when (i < 0 || i >= n)
        $ error $ "setValueAtIndex: index " ++ show i ++ " is out of range"
    unsafePrimToPrim $ c_setValueAtIndex a i x

{#fun CFArrayInsertValueAtIndex as c_insertValueAtIndex
    `Object o' => 
    { withObject* `Mutable s (Array o)'
    , `Int'
    , withVoidObject* `o'
    } -> `()' #}

-- | Insert a value into an array at the given index.
--
-- The index must be between 0 and N (inclusive), where N is the current
-- size of the array.
insertValueAtIndex :: (PrimMonad m, Object o) => MArray (PrimState m) o -> Int -> o -> m ()
insertValueAtIndex a i x = do
    n <- getCount a
    when (i < 0 || i > n)
        $ error $ "insertValueAtIndex: index " ++ show i ++ " is out of range"
    unsafePrimToPrim $ c_insertValueAtIndex a i x

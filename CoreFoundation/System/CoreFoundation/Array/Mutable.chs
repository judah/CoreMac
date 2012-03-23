-- | Core Foundation mutable arrays.  These are toll-free bridged with
-- `NSMutableArray'.
module System.CoreFoundation.Array.Mutable(
                            newMutableArray,
                            appendValue,
                            setValueAtIndex,
                            insertValueAtIndex,
                            ) where

import Foreign.Ptr
import Foreign.C
import Control.Monad (when)

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Array

#include <CoreFoundation/CoreFoundation.h>

{#pointer CFMutableArrayRef as ArrayRef nocode #}
foreign import ccall "&" kCFTypeArrayCallBacks :: Ptr ()

-- | Create a new mutable array.  The array starts empty and can contain up to the
-- given number of values.
{#fun CFArrayCreateMutable as cfNewMutableArray
    { withDefaultAllocator- `AllocatorPtr'
    , `Int'
    , '($ kCFTypeArrayCallBacks)'- `Ptr ()'
    } -> `ArrayRef' id#}

newMutableArray :: Object a => Int -> IO (Mutable (Array a))
newMutableArray n = fmap unsafeMutable . getOwned $ cfNewMutableArray n

{#fun CFArrayAppendValue as appendValue
    `Object o' => { '(withObject . unMutable)'* `Mutable (Array o)'
    , withVoidObject* `o'
    } -> `()' #}

{#fun CFArraySetValueAtIndex as c_setValueAtIndex
    `Object o' =>
    { '(withObject . unMutable)'* `Mutable (Array o)'
    , `Int'
    , withVoidObject* `o'
    } -> `()' #}

-- | Change the value at the given index in the array.
--
-- The index must be less than the current size of the array.
setValueAtIndex :: Object o => Mutable (Array o) -> Int -> o -> IO ()
setValueAtIndex a i x = do
    n <- getCount $ unMutable a
    when (i < 0 || i >= n)
        $ error $ "setValueAtIndex: index " ++ show i ++ " is out of range"
    c_setValueAtIndex a i x

{#fun CFArrayInsertValueAtIndex as c_insertValueAtIndex
    `Object o' => 
    { '(withObject . unMutable)'* `Mutable (Array o)'
    , `Int'
    , withVoidObject* `o'
    } -> `()' #}

-- | Insert a value into an array at the given index.
--
-- The index must be between 0 and N (inclusive), where N is the current
-- size of the array.
insertValueAtIndex :: Object o => Mutable (Array o) -> Int -> o -> IO ()
insertValueAtIndex a i x = do
    n <- getCount $ unMutable a
    when (i < 0 || i > n)
        $ error $ "insertValueAtIndex: index " ++ show i ++ " is out of range"
    c_insertValueAtIndex a i x

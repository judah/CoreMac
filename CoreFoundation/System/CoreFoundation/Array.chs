-- | Core Foundation Arrays.  These are toll-free bridged with 'NSArray'.
module System.CoreFoundation.Array(
                    Array,
                    ArrayRef,
                    CFArray,
                    -- * Accessing elements
                    getCount,
                    getObjectAtIndex,
                    -- * Conversion
                    fromVector,
                    toVector,
                    fromList,
                    toList,
                    -- * Marshalling assistance
                    getOwnedArray,
                    ) where


import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import System.CoreFoundation.Array.Internal
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.Unsafe(TypeID(..))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

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
#include "array.h"

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

-- | Build an array from a list
fromList :: Object a => [a] -> Array a
fromList = fromVector . V.fromList
                        
-- | Convert the array to a list
toList :: Object a => Array a -> [a]
toList = V.toList . toVector

-- | Build an array from a vector
fromVector :: Object a => V.Vector a -> Array a
fromVector v = 
    unsafePerformIO $ 
    withVector v $ \buf len ->
    getOwned $
    cfArrayCreate (castPtr buf) (fromIntegral len) kCFTypeArrayCallBacks

-- | Convert the array to a vector
toVector :: Object a => Array a -> V.Vector a
toVector a = 
    unsafePerformIO $
    withObject a $ \p -> do
      let len = getCount a
      (res, _) <- buildVector len $ \buf ->
        {#call unsafe hsCFArrayGetValues#} p (fromIntegral len) (castPtr buf)
      return res

------ Marshalling
{- |
CoreFoundation represents empty arrays by null pointers, which
may not be passed to 'getOwned'. Instead, use this scheme for wrapping
arrays, which ensures that the resulting 'ArrayRef' is non-null.
-}
getOwnedArray :: Object a => IO ArrayRef -> IO (Array a)
getOwnedArray = fmap (fromMaybe (fromVector V.empty)) . maybeGetOwned

-- | Core Foundation Dictionaries.  They are toll-free bridged with 'NSDictionary'.
module System.CoreFoundation.Dictionary(
                    Dictionary,
                    DictionaryRef,
                    -- * Accessing elements
                    getValueCount,
                    getValue,
                    -- * Conversions
                    fromKeyValues,
                    toKeyValues,
                    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign (withArray, withArrayLen)
import System.IO.Unsafe (unsafePerformIO)
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.Array.Internal
import qualified Data.Vector as V

{- |
The CoreFoundation @CFDictionary@ type.
-}
data CFDictionary
{- |
A dictionary with keys of type @k@ and values of type @v@. Wraps
@CFDictionaryRef@.
-}
newtype Dictionary k v = Dictionary { unDictionary :: ForeignPtr CFDictionary }

-- | The CoreFoundation @CFDictionaryRef@ type.
{#pointer CFDictionaryRef as DictionaryRef -> CFDictionary#}

instance Object (Dictionary k v) where
  type Repr (Dictionary k v) = CFDictionary
  unsafeObject = Dictionary
  unsafeUnObject = unDictionary
  maybeStaticTypeID _ = Just _CFDictionaryGetTypeID

foreign import ccall "CFDictioanryGetTypeID" _CFDictionaryGetTypeID :: TypeID
instance StaticTypeID (Dictionary k v) where
  unsafeStaticTypeID _ = _CFDictionaryGetTypeID

#include <CoreFoundation/CoreFoundation.h>

{#fun pure unsafe CFDictionaryGetCount as getValueCount
    { withObject* `Dictionary k v' } -> `Int' #}

-- TODO: allow any old type as key?

{#fun unsafe CFDictionaryGetValue as cfGetValue
    `(Object k, Object v)' => { withObject* `Dictionary k v' 
    , withVoidObject* `k'
    } -> `Ptr ()' id #}

getValue :: (Object k, Object v) => Dictionary k v -> k -> Maybe v
getValue dict k = unsafePerformIO . maybeGetOwned . fmap castPtr $ cfGetValue dict k

-- There's subtlety around GetValueForKey returning NULL; see the docs.
-- For now, we'll assume it acts like NSDocument and doesn't have nil values.

foreign import ccall "&" kCFTypeDictionaryKeyCallBacks :: Ptr ()
foreign import ccall "&" kCFTypeDictionaryValueCallBacks :: Ptr ()

{#fun CFDictionaryCreate as cfDictionaryCreate
    { withDefaultAllocator- `AllocatorPtr'
    , id `Ptr (Ptr ())'
    , id `Ptr (Ptr ())'
    , `Int'
    , id `Ptr ()'
    , id `Ptr ()'
    } -> `DictionaryRef' id #}

-- | Create a new immutable 'Dictionary' whose keys and values are taken from the given
-- vector.
fromKeyValues :: (Object k, Object v) => V.Vector (k, v) -> Dictionary k v
fromKeyValues kvs =
  let (keys, vals) = V.unzip kvs in
  unsafePerformIO $
  withVector keys $ \pk len ->
  withVector vals $ \pv _ ->
  getOwned $ 
  cfDictionaryCreate (castPtr pk) (castPtr pv) len
    kCFTypeDictionaryKeyCallBacks
    kCFTypeDictionaryValueCallBacks

toKeyValues :: (Object k, Object v) => Dictionary k v -> V.Vector (k, v)
toKeyValues d =
  uncurry V.zip $
  unsafePerformIO $
  withObject d $ \p ->
    let len = getValueCount d in
    buildVector len $ \kp ->
      fst `fmap` (buildVector len $ \vp ->
          {#call unsafe CFDictionaryGetKeysAndValues as ^ #} 
             p 
             (castPtr kp) 
             (castPtr vp)
         )

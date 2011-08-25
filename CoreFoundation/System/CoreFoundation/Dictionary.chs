-- | Core Foundation Dictionaries.  They are toll-free bridged with 'NSDictionary'.
module System.CoreFoundation.Dictionary(
                    Dictionary,
                    DictionaryRef,
                    -- * Accessing elements
                    getValueCount,
                    getValue,
                    getValueOfType,
                    -- * Creating dictionaries
                    fromKeyValues,
                    ) where

import Foreign
import Foreign.C
import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

declareCFType "Dictionary"

#include <CoreFoundation/CoreFoundation.h>

{#fun unsafe CFDictionaryGetCount as getValueCount
    { withObject* `Dictionary' } -> `Int' #}

-- TODO: allow any old type as key?

{#fun unsafe CFDictionaryGetValue as getValue
    `(Object key, Object value)' => { withObject* `Dictionary' 
    , withObject* `key'
    } -> `Maybe value' maybeGetAndRetain* #}

-- There's subtlety around GetValueForKey returning NULL; see the docs.
-- For now, we'll assume it acts like NSDocument and doesn't have nil values.

-- | Returns Nothing if the key was not found in the 'Dictionary', or if the corresponding
-- value is of an incompatible type.
getValueOfType :: (Object key, Object value) => Dictionary -> key -> IO (Maybe value)
getValueOfType d k = do
                mObject <- getValue d k
                return $ mObject >>= castObject
                    

foreign import ccall "&" kCFTypeDictionaryKeyCallBacks :: Ptr ()
foreign import ccall "&" kCFTypeDictionaryValueCallBacks :: Ptr ()

{#fun CFDictionaryCreate as cfDictionaryCreate
    { withDefaultAllocator- `AllocatorPtr'
    , id `Ptr CFTypeRef'
    , id `Ptr CFTypeRef'
    , `Int'
    , id `Ptr ()'
    , id `Ptr ()'
    } -> `Dictionary' getOwned* #}

-- | Create a new immutable 'Dictionary' whose keys and values are taken from the given
-- list.
fromKeyValues :: (Object key, Object value) => [(key,value)] -> Dictionary
fromKeyValues kvs = unsafePerformIO $ do
    let (keys,values) = unzip kvs
    withObjects keys $ \ks -> do
    withArrayLen ks $ \n pks -> do
    withObjects values $ \vs -> do
    withArray vs $ \pvs -> do
    cfDictionaryCreate pks pvs n
        kCFTypeDictionaryKeyCallBacks
        kCFTypeDictionaryValueCallBacks

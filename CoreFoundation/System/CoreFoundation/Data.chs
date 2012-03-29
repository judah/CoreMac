-- | Interface to CoreFoundation's @CFData@ C type.  It is toll-free bridged with the @NSData@ type.
module System.CoreFoundation.Data(
                Data,
                DataRef,
                getLength,
                fromByteString,
                getByteString,
                ) where

-- TODO:
--  - lazy bytestrings?
--  - Int (haskell) index vs Int32 (CoreFoundatino) index?
--  - some safe way to use CFDataCreateWithBytesNoCopy
--  - CFDataCreate is immutable, use it to make funcs pure?
--  - Mutable data?

import Foreign.Ptr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as UnsafeB
import           Data.ByteString.Char8()
import Control.Exception (finally)
import Data.Word (Word8)
import Data.Typeable
import Data.String
import Control.DeepSeq

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH


#include <CoreFoundation/CoreFoundation.h>

declareCFType "Data"
{#pointer CFDataRef as DataRef nocode#}

{#fun unsafe CFDataGetBytePtr as getBytePtr
    { withObject* `Data'
    } -> `Ptr Word8' castPtr #}

-- | Returns the number of bytes stored in the 'Data' object.
{#fun pure unsafe CFDataGetLength as getLength
    { withObject* `Data'
    } -> `CFIndex' id #}

{#fun unsafe CFDataCreate as cfCreateData
    { withDefaultAllocator- `AllocatorPtr',
      castPtr `Ptr Word8',
      id `CFIndex'
    } -> `DataRef' id #}


-- | Directly access the internal bytes of the Data.
-- It is not safe to use the 'Ptr' outside of the block.
withData :: Data -> (Ptr Word8 -> CFIndex -> IO a) -> IO a
withData d f = do
    p <- getBytePtr d
    let len = getLength d
    f p len `finally` touchObject d

-- | Makes a new copy of the given data.
getByteString :: Data -> B.ByteString
getByteString d = unsafePerformIO $ withData d $ \p n -> B.packCStringLen (castPtr p, cvtEnum n)

-- | Makes a new immutable Data object with a copy of the ByteString's data.
fromByteString :: B.ByteString -> Data
fromByteString b = unsafePerformIO $ UnsafeB.unsafeUseAsCStringLen b $ \(p,len) ->
                        getOwned $ cfCreateData (castPtr p) (toEnum len)

deriving instance Typeable Data
instance Show Data where
  show = show . getByteString
instance Eq Data where
  a == b = getByteString a == getByteString b
instance Ord Data where
  compare a b = compare (getByteString a) (getByteString b)
instance IsString Data where
  fromString = fromByteString . fromString
instance NFData Data

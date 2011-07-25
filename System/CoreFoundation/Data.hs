-- | Interface to the CFData type.  It is toll-free bridged with NSString.
module System.CoreFoundation.Data where

-- TODO:
--  - lazy bytestrings?
--  - Int (haskell) index vs Int32 (CoreFoundatino) index?
--  - some safe way to use CFDataCreateWithBytesNoCopy
--  - CFDataCreate is immutable, use it to make funcs pure?
--  - Mutable data?

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.TH

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as UnsafeB

declareCFType "Data"

unsafeForeignImport "CFDataGetBytePtr" [t| DataRef -> IO (Ptr Word8)|]

unsafeForeignImport "CFDataGetLength" [t| DataRef -> IO CFIndex |]

unsafeForeignImport "CFDataCreate"
    [t| CFAllocatorRef -> Ptr CChar -> CFIndex -> IO DataRef |]

-- | Directly access the internal bytes of the Data.
-- It is not safe to use the CStringLen outside of the block.
withData :: Data -> (CStringLen -> IO a) -> IO a
withData d f = withCF d $ \dp -> do
    p <- c_CFDataGetBytePtr dp
    len <- c_CFDataGetLength dp
    f (castPtr p, fromEnum len) -- Word8 and CChar have same foreign rep

-- | Makes a new copy of the given data.
dataToByteString :: Data -> IO B.ByteString
dataToByteString d = withData d B.packCStringLen

-- | Makes a new Data object with a copy of the ByteString's data.
dataFromByteString :: B.ByteString -> IO Data
dataFromByteString b = UnsafeB.unsafeUseAsCStringLen b $ \(p,len) ->
                        c_CFDataCreate defaultAllocatorRef p (toEnum len)
                            >>= created

-- | Get the length of a Data object.
dataLength :: Data -> IO Int
dataLength d = withCF d $ fmap fromEnum . c_CFDataGetLength

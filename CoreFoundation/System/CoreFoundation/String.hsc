-- | Interface to CoreFoundation's @CFString@ type.  It is toll-free bridged with the @NSString@ class.
module System.CoreFoundation.String(
                String,
                StringRef,
                stringFromText,
                stringToText,
                ) where

-- TODO: 
--  - Mutable?
--  - More efficient marshalling (e.g. stringFromText copies twice)
--    (maybe through ByteStrings)
--  - Convert to/from Text and also Prelude.String
--  - Use CFStringInlineBuffer
--  - endian-ness is not portable.  
--    Could use NSByteOrder to figure out Text's endianness...but
--    probably kCFStringEncodingUTF16 works fine.  Just need to stop
--    using a DataRef.

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.Data

import Prelude hiding (String)
import qualified Prelude as P

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Foreign (useAsPtr, fromPtr)

#include <CoreFoundation/CoreFoundation.h>

declareCFType "String"

type CFStringEncoding = #type CFStringEncoding

kCFStringEncodingUTF16 :: CFStringEncoding
kCFStringEncodingUTF16 = #const kCFStringEncodingUTF16

kCFStringEncodingUTF16LE :: CFStringEncoding
kCFStringEncodingUTF16LE = #const kCFStringEncodingUTF16LE

kCFStringEncodingUTF16BE :: CFStringEncoding
kCFStringEncodingUTF16BE = #const kCFStringEncodingUTF16BE

unsafeForeignImport "CFStringCreateWithBytes"
    [t| AllocatorRef -> Ptr Word8 -> CFIndex
                                    -> CFStringEncoding
                                    -> CBoolean -> IO StringRef |]

unsafeForeignImport "CFStringCreateExternalRepresentation"
    [t| AllocatorRef
                -> StringRef -> CFStringEncoding -> Word8
                -> IO DataRef |]



-- TODO: endianness?
-- | Create a copy of the Text in a CF.String.
stringFromText :: Text.Text -> IO String
stringFromText t = useAsPtr t $ \p len -> do
                    c_CFStringCreateWithBytes defaultAllocatorRef (castPtr p)
                        (2 * (toEnum $ fromEnum len)) kCFStringEncodingUTF16
                        0 -- Text doesn't add a BOM
                     >>= getOwned

-- | Copies the String into Text.
stringToText :: String -> IO Text.Text
stringToText s = withCF s $ \sp -> do
    d <- c_CFStringCreateExternalRepresentation defaultAllocatorRef sp
                -- Force endian-ness so it doesn't output a bom
                kCFStringEncodingUTF16LE
                (toEnum $ fromEnum '?') -- shouldn't be needed
            >>= getOwned
    fmap Encoding.decodeUtf16LE $ dataToByteString d
    
    


-- | Interface to the CFString type, which is toll-free bridged with NSString.
module System.CoreFoundation.String where

-- TODO: 
--  - Mutable?
--  - More efficient marshalling (e.g. stringFromText copies twice)
--    (maybe through ByteStrings)
--  - Convert to/from Text and also Prelude.String
--  - Use CFStringInlineBuffer
--  - endian-ness

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Data

import Prelude hiding (String)
import qualified Prelude as P

import qualified Data.Text as Text
import Data.Text.Foreign (useAsPtr, fromPtr)

#include <CoreFoundation/CFString.h>


newtype String = String (ForeignPtr ())
type CFStringRef = Ptr ()

instance CFType String where
    cftype = String
    uncftype (String p) = p

type CFStringEncoding = #type CFStringEncoding

kCFStringEncodingUTF16 :: CFStringEncoding
kCFStringEncodingUTF16 = #const kCFStringEncodingUTF16

kCFStringEncodingUTF16LE :: CFStringEncoding
kCFStringEncodingUTF16LE = #const kCFStringEncodingUTF16LE

kCFStringEncodingUTF16BE :: CFStringEncoding
kCFStringEncodingUTF16BE = #const kCFStringEncodingUTF16BE

foreign import ccall unsafe "CFStringCreateWithBytes"
    c_CFStringCreateWithBytes :: CFAllocatorRef -> Ptr Word8 -> CFIndex
                                    -> CFStringEncoding
                                    -> CBoolean -> IO CFStringRef

foreign import ccall unsafe "CFStringCreateExternalRepresentation"
    c_CFStringCreateExternalRepresentation :: CFAllocatorRef
                -> CFStringRef -> CFStringEncoding -> Word8
                -> IO CFDataRef



-- TODO: endianness?
-- | Create a copy of the Text in a CF.String.
stringFromText :: Text.Text -> IO String
stringFromText t = useAsPtr t $ \p len -> do
                    c_CFStringCreateWithBytes defaultAllocatorRef (castPtr p)
                        (2 * (toEnum $ fromEnum len)) kCFStringEncodingUTF16
                        0 -- Text doesn't add a BOM
                     >>= retainOrError "stringFromText: couldn't create String."

-- | Copies the String into Text.
stringToText :: String -> IO Text.Text
stringToText s = cfWith s $ \sp -> do
    dp <- c_CFStringCreateExternalRepresentation defaultAllocatorRef sp
                -- Force endian-ness so it doesn't output a bom
                -- TODO: breaks on big-endian architectures,
                -- since Text's encoding is platform-dependent
                kCFStringEncodingUTF16LE
                (toEnum $ fromEnum '?') -- shouldn't be needed
    p <- c_CFDataGetBytePtr dp
    len <- c_CFDataGetLength dp
    t <- fromPtr (castPtr p) (toEnum $ fromEnum $ len `div` 2)
    cfRelease dp
    return t
    
    


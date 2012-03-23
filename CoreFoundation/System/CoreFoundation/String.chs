-- | Interface to CoreFoundation's @CFString@ type.  It is toll-free bridged with the @NSString@ class.
module System.CoreFoundation.String(
                String,
                StringRef,
                -- * Conversion to/from 'Data'
                newExternalRepresentation,
                newStringFromExternalRepresentation,
                StringEncoding(..),
                -- * Conversion to/from 'Prelude.String'
                fromChars,
                getChars,
                -- * Conversion to/from 'Text'
                fromText,
                getText,
                -- * Foreign import of string constants
                importCFString,
                importCFStringAs,
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

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array (allocaArray)
import Foreign (fromBool, peek)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.String.TH
{#import System.CoreFoundation.Data#}

import Prelude hiding (String)
import qualified Prelude

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Foreign (useAsPtr, fromPtr)
import Language.Haskell.TH

#include <CoreFoundation/CoreFoundation.h>

{#pointer CFStringRef as StringRef nocode#}

{#enum define StringEncoding
    { kCFStringEncodingMacRoman as MacRoman,
       kCFStringEncodingWindowsLatin1 as WindowsLatin1,
       kCFStringEncodingISOLatin1 as ISOLatin1,
       kCFStringEncodingNextStepLatin as NextStepLatin,
       -- kCFStringEncodingUnicode as Unicode, -- dupe of UTF16
       kCFStringEncodingUTF8 as UTF8,
       kCFStringEncodingNonLossyASCII as NonLossyASCII,
       kCFStringEncodingUTF16 as UTF16,
       kCFStringEncodingUTF16BE as UTF16BE,
       kCFStringEncodingUTF16LE as UTF16LE,
       kCFStringEncodingUTF32 as UTF32,
       kCFStringEncodingUTF32BE as UTF32BE,
       kCFStringEncodingUTF32LE as UTF32LE
    } #}

{#fun unsafe CFStringCreateWithBytes as createStringWithBytes
    { withDefaultAllocator- `AllocatorPtr',
      castPtr `Ptr Word8',
      toEnum `Int',
      cvtEnum `StringEncoding',
      `Bool'
    } -> `StringRef' id #}

{#fun unsafe CFStringCreateExternalRepresentation as newExternalRepresentation
    { withDefaultAllocator- `AllocatorPtr',
      withObject* `String',
      cvtEnum `StringEncoding',
      cvtEnum `Word8'
    } -> `DataRef' id #}

{#fun unsafe CFStringCreateFromExternalRepresentation as newStringFromExternalRepresentation
    { withDefaultAllocator- `AllocatorPtr',
      withObject* `Data',
      cvtEnum `StringEncoding'
    } -> `StringRef' id #}



-- | Create a new immutable @CoreFoundation.String@ which contains a copy of the given 'Text'.
fromText :: Text.Text -> String
fromText t = unsafePerformIO $ useAsPtr t $ \p len -> getOwned $
                        createStringWithBytes (castPtr p)
                            (cvtEnum $ 2*len) UTF16
                            False -- Text doesn't add a BOM

-- | Create a new immutable @CoreFoundation.String@ which contains a copy of the given @Prelude.String@.
fromChars :: Prelude.String -> String
fromChars = fromText . Text.pack

-- | Extract a 'Text' copy of the given @CoreFoundation.String@.
getText :: String -> IO Text.Text
getText str =
    withObject str $ \str_p -> do
      len <- {#call unsafe CFStringGetLength as ^ #} str_p
      ptr <- {#call unsafe CFStringGetCharactersPtr as ^ #} str_p
      if ptr /= nullPtr
        then fromPtr (castPtr ptr) (fromIntegral len)
        else allocaArray (fromIntegral len) $ \out_ptr -> do
          {#call unsafe hsCFStringGetCharacters as ^ #} str_p len out_ptr
          fromPtr (castPtr out_ptr) (fromIntegral len)

-- | Extract a 'Prelude.String' copy of the given @CoreFoundation.String@.
getChars :: String -> IO Prelude.String
getChars = fmap Text.unpack . getText



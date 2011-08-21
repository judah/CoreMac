-- | Interface to CoreFoundation's @CFString@ type.  It is toll-free bridged with the @NSString@ class.
module System.CoreFoundation.String(
                String,
                StringRef,
                -- * Conversion to/from 'Data'
                newExternalRepresentation,
                newStringFromExternalRepresentation,
                StringEncoding(..),
                -- * Conversion to/from 'Prelude.String'
                newStringFromChars,
                getChars,
                -- * Conversion to/from 'Text'
                newStringFromText,
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

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.Data

import Prelude hiding (String)
import qualified Prelude

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Foreign (useAsPtr, fromPtr)
import Language.Haskell.TH

#include <CoreFoundation/CoreFoundation.h>

declareCFType "String"

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
    } -> `String' getOwned* #}

{#fun unsafe CFStringCreateExternalRepresentation as newExternalRepresentation
    { withDefaultAllocator- `AllocatorPtr',
      withObject* `String',
      cvtEnum `StringEncoding',
      cvtEnum `Word8'
    } -> `Data' getOwned* #}

{#fun unsafe CFStringCreateFromExternalRepresentation as newStringFromExternalRepresentation
    { withDefaultAllocator- `AllocatorPtr',
      withObject* `Data',
      cvtEnum `StringEncoding'
    } -> `String' getOwned* #}



-- | Create a new @CoreFoundation.String@ which contains a copy of the given 'Text'.
newStringFromText :: Text.Text -> IO String
newStringFromText t = useAsPtr t $ \p len ->
                        createStringWithBytes (castPtr p)
                            (cvtEnum $ 2*len) UTF16
                            False -- Text doesn't add a BOM

-- | Create a new @CoreFoundation.String@ which contains a copy of the given @Prelude.String@.
newStringFromChars :: Prelude.String -> IO String
newStringFromChars = newStringFromText . Text.pack

-- | Extract a 'Text' copy of the given @CoreFoundation.String@.
getText :: String -> IO Text.Text
getText s = do
    d <- newExternalRepresentation s UTF16LE (cvtEnum '?')
    fmap Encoding.decodeUtf16LE $ getByteString d

-- | Extract a 'Prelude.String' copy of the given @CoreFoundation.String@.
getChars :: String -> IO Prelude.String
getChars = fmap Text.unpack . getText


importCFStringAs :: Prelude.String -> Prelude.String -> Q [Dec]
importCFStringAs foreignStr nameStr = do
    ptrName <- newName (nameStr ++ "Ptr")
    let name = mkName nameStr
    ptrType <- [t| Ptr CFTypeRef |]
    expr <- [| unsafePerformIO $ peek $(varE ptrName) >>= getAndRetain |]
    return
        [ ForeignD $ ImportF CCall Safe ("&" ++ foreignStr) ptrName ptrType
        , SigD name (ConT ''String)
        , FunD name [Clause [] (NormalB expr) []]
        ]


importCFString :: Prelude.String -> Q [Dec]
importCFString s = importCFStringAs s s

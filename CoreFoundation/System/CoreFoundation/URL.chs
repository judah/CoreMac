-- | Core Foundation URLs.  These are toll-free bridged with @NSURL@.
module System.CoreFoundation.URL(
                URL,
                URLRef,
                CFURL,
                -- * Converting to 'String'
                urlFromString,
                urlToString,
                -- * Utilities
                absoluteURL,
                fileSystemPath,
                PathStyle(..),
                ) where

import Foreign.Ptr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
{#import System.CoreFoundation.String#}
import Prelude hiding (String)
import Data.Typeable
import Control.DeepSeq

#include <CoreFoundation/CoreFoundation.h>

declareCFType "URL"
{#pointer CFURLRef as URLRef nocode#}

{#enum define PathStyle
    { kCFURLPOSIXPathStyle as POSIXPathStyle
    , kCFURLHFSPathStyle as HFSPathStyle
    , kCFURLWindowsPathStyle as WindowsPathStyle
    } #}

-- | Creates a URL from the given String. Any escape sequences will be interpreted using UTF-8.
urlFromString ::
    String -- ^ The String to be parsed.
 -> Maybe URL -- ^ The URL to which the String is relative.
 -> URL
urlFromString str murl = unsafePerformIO $ getOwned $ cfUrlFromString str murl

{#fun CFURLCreateWithString as cfUrlFromString
  { withDefaultAllocator- `AllocatorPtr', withObject* `String', withMaybeObject* `Maybe URL' } -> `URLRef' id #}

-- | Returns a 'String' representation of the 'URL'.
{#fun pure CFURLGetString as urlToString
  { withObject* `URL' } -> `String' getAndRetain* #}

{#fun CFURLCopyFileSystemPath as cfFileSystemPath
    { withObject* `URL' 
    , cvtEnum `PathStyle'
    } -> `StringRef' id #}

{- | Returns the path portion of the given URL. -}
fileSystemPath :: URL -> PathStyle -> String
fileSystemPath url style = unsafePerformIO $ getOwned $ cfFileSystemPath url style

{#fun CFURLCopyAbsoluteURL as cfAbsoluteURL
    { withObject* `URL' } -> `URLRef' id #} 

{- | Creates a new CFURL object by resolving the relative portion of a URL against its base. Returns Nothing if the URL cannot be made absolute. -}
absoluteURL :: URL -> Maybe URL
absoluteURL url = unsafePerformIO $ maybeGetOwned $ cfAbsoluteURL url

deriving instance Typeable URL
instance NFData URL
instance Eq URL where
  a == b = equal a b
instance Show URL where
  show = getChars . urlToString

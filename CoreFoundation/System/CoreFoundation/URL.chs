-- | Core Foundation URLs.  These are toll-free bridged with @NSURL@.
module System.CoreFoundation.URL(
                URL,
                URLRef,
                absoluteURL,
                fileSystemPath,
                PathStyle(..),
                ) where

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

{#fun CFURLCopyFileSystemPath as cfFileSystemPath
    { withObject* `URL' 
    , cvtEnum `PathStyle'
    } -> `StringRef' id #}

fileSystemPath :: URL -> PathStyle -> String
fileSystemPath url style = unsafePerformIO $ getOwned $ cfFileSystemPath url style

{#fun CFURLCopyAbsoluteURL as cfAbsoluteURL
    { withObject* `URL' } -> `URLRef' id #} 

absoluteURL :: URL -> URL
absoluteURL url = unsafePerformIO $ getOwned $ cfAbsoluteURL url

deriving instance Typeable URL
instance NFData URL
instance Eq URL where
  a == b = equal a b
instance Show URL where
  show = unsafePerformIO . getObjectDescription

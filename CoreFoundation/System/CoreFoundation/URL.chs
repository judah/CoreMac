-- | Core Foundation URLs.  These are toll-free bridged with @NSURL@.
module System.CoreFoundation.URL(
                URL,
                URLRef,
                absoluteURL,
                fileSystemPath,
                PathStyle(..),
                ) where

import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.String
import Prelude hiding (String)

#include <CoreFoundation/CoreFoundation.h>

declareCFType "URL"

{#enum define PathStyle
    { kCFURLPOSIXPathStyle as POSIXPathStyle
    , kCFURLHFSPathStyle as HFSPathStyle
    , kCFURLWindowsPathStyle as WindowsPathStyle
    } #}

{#fun pure CFURLCopyFileSystemPath as fileSystemPath
    { withObject* `URL' 
    , cvtEnum `PathStyle'
    } -> `String' getOwned* #}

{#fun pure CFURLCopyAbsoluteURL as absoluteURL
    { withObject* `URL' } -> `URL' getOwned* #} 


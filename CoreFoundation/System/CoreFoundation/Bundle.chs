-- | Core Foundation Bundles.  These are *not* toll-free bridged with the @NSBundle@ class,
-- but do provide much of the same functionality.
module System.CoreFoundation.Bundle(
                    Bundle,
                    BundleRef,
                    getMainBundle,
                    getExecutableURL,
                    getResourceURL,
                    getAuxiliaryExecutableURL,
                    ) where

import Foreign.Ptr

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
{#import System.CoreFoundation.URL#}
{#import System.CoreFoundation.String#}
import Prelude hiding (String)

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Bundle"
{#pointer CFBundleRef as BundleRef nocode#}

{#fun CFBundleGetMainBundle as getMainBundle
    { } -> `Bundle' getAndRetain* #}

{#fun CFBundleCopyExecutableURL as cfGetExecutableURL
    { withObject* `Bundle' } -> `URLRef' id #}

getExecutableURL :: Bundle -> IO URL
getExecutableURL bundle = getOwned $ cfGetExecutableURL bundle

{#fun CFBundleCopyAuxiliaryExecutableURL as cfGetAuxiliaryExecutableURL
    { withObject* `Bundle' 
    , withObject* `String' } -> `URLRef' id #}

getAuxiliaryExecutableURL :: Bundle -> String -> IO URL
getAuxiliaryExecutableURL bundle str = getOwned $ cfGetAuxiliaryExecutableURL bundle str

{#fun CFBundleCopyResourceURL as cfGetResourceURL
    { withObject* `Bundle'
    , withObject* `String'
    , withMaybeObject* `Maybe String'
    , withMaybeObject* `Maybe String'
    } -> `URLRef' id #}

getResourceURL :: Bundle -> String -> Maybe String -> Maybe String -> IO URL
getResourceURL a b c d = getOwned $ cfGetResourceURL a b c d

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
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.URL
import System.CoreFoundation.String
import Prelude hiding (String)

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Bundle"

{#fun CFBundleGetMainBundle as getMainBundle
    { } -> `Bundle' getAndRetain* #}

{#fun CFBundleCopyExecutableURL as getExecutableURL
    { withObject* `Bundle' } -> `URL' getOwned* #}

{#fun CFBundleCopyAuxiliaryExecutableURL as getAuxiliaryExecutableURL
    { withObject* `Bundle' 
    , withObject* `String' } -> `URL' getOwned* #}

{#fun CFBundleCopyResourceURL as getResourceURL
    { withObject* `Bundle'
    , withObject* `String'
    , withMaybeObject* `Maybe String'
    , withMaybeObject* `Maybe String'
    } -> `URL' getOwned* #}

withMaybeObject Nothing = ($ nullPtr)
withMaybeObject (Just o) = withObject o

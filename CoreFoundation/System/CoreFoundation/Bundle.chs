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

import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import System.CoreFoundation.Base
{#import System.CoreFoundation.URL#}
{#import System.CoreFoundation.String#}
import Prelude hiding (String)
import Data.Typeable
import Control.DeepSeq

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Bundle"
{#pointer CFBundleRef as BundleRef nocode#}

{- |
Returns an application’s main bundle.
-}
{#fun CFBundleGetMainBundle as getMainBundle
    { } -> `Bundle' getAndRetain* #}

{#fun CFBundleCopyExecutableURL as cfGetExecutableURL
    { withObject* `Bundle' } -> `URLRef' id #}

{- |
Returns the location of a bundle’s main executable code.
-}
getExecutableURL :: Bundle -> IO URL
getExecutableURL bundle = getOwned $ cfGetExecutableURL bundle

{#fun CFBundleCopyAuxiliaryExecutableURL as cfGetAuxiliaryExecutableURL
    { withObject* `Bundle' 
    , withObject* `String' } -> `URLRef' id #}

{- |
Returns the location of a bundle’s auxiliary executable code.

  [Discussion] This function can be used to find executables other than your main executable. This is useful, for instance, for applications that have some command line tool that is packaged with and used by the application. The tool can be packaged in the various platform executable directories in the bundle and can be located with this function. This allows an application to ship versions of the tool for each platform as it does for the main application executable.

-}
getAuxiliaryExecutableURL :: Bundle -> String -> IO URL
getAuxiliaryExecutableURL bundle str = getOwned $ cfGetAuxiliaryExecutableURL bundle str

{#fun CFBundleCopyResourceURL as cfGetResourceURL
    { withObject* `Bundle'
    , withObject* `String'
    , withMaybeObject* `Maybe String'
    , withMaybeObject* `Maybe String'
    } -> `URLRef' id #}

{- |
Returns the location of a resource contained in the specified bundle.

For example, if a bundle contains a subdirectory WaterSounds that includes a file Water1.aiff, you can retrieve the URL for the file using

> getResourceURL bundle "Water1" (Just "aiff") (Just "WaterSounds")

-}
getResourceURL :: 
     Bundle
     -- ^ The bundle to examine.

  -> String 
     -- ^ The name of the requested resource
  -> Maybe String 
     -- ^ The abstract type of the requested resource. The type is expressed as a filename extension, such as jpg. Pass @Nothing@ if you don't need to search by type.

  -> Maybe String
     -- ^ The name of the subdirectory of the bundle's resources directory to search. Pass @Nothing@ to search the standard Bundle resource locations.

  -> IO URL
getResourceURL a b c d = getOwned $ cfGetResourceURL a b c d

deriving instance Typeable Bundle
instance NFData Bundle
deriving instance Eq Bundle
deriving instance Ord Bundle
instance Show Bundle where
  show = unsafePerformIO . getObjectDescription

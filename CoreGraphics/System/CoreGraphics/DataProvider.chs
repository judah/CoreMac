{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider(
                    DataProvider,
                    DataProviderRef,
                    dataProviderWithFilename,
                    ) where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

#include <ApplicationServices/ApplicationServices.h>

declareCFType "DataProvider"

{#fun unsafe CGDataProviderCreateWithFilename as dataProviderWithFilename
    { withCString* `FilePath'
    } -> `DataProvider' getOwned* #}

{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider(
                    DataProvider,
                    DataProviderRef,
                    newDataProviderFromFile,
                    ) where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

#include <ApplicationServices/ApplicationServices.h>

declareCFType "DataProvider"

{#fun unsafe CGDataProviderCreateWithFilename as newDataProviderFromFile
    { withCString* `FilePath'
    } -> `DataProvider' getOwned* #}

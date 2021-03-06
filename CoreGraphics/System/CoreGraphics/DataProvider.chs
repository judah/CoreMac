{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider(
                    DataProvider,
                    DataProviderRef,
                    newDataProviderFromFile,
                    ) where

import Foreign.Ptr
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH

#include <ApplicationServices/ApplicationServices.h>

declareCFTypeAs "CGDataProvider" "DataProvider"

{#fun unsafe CGDataProviderCreateWithFilename as newDataProviderFromFile
    { withCString* `FilePath'
    } -> `DataProvider' getOwned* #}

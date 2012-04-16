{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider(
                    DataProvider,
                    DataProviderRef,
                    CGDataProvider,
                    newDataProviderFromFile,
                    ) where

import Foreign.Ptr
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH

#include <ApplicationServices/ApplicationServices.h>

declareCFTypeAs "CGDataProvider" "DataProvider"
{#pointer CGDataProviderRef as DataProviderRef nocode#}

newDataProviderFromFile :: FilePath -> IO DataProvider
newDataProviderFromFile file = getOwned $ cgNewDataProviderFromFile file

{#fun unsafe CGDataProviderCreateWithFilename as cgNewDataProviderFromFile
    { withCString* `FilePath'
    } -> `DataProviderRef' id #}

-- c2hs pointer hooks for CoreFoundation
{#pointer CFArrayRef as ArrayRef nocode#}
{#pointer CFMutableArrayRef as MArrayRef nocode #}
{#pointer CFTypeRef nocode#}
{#pointer CFBooleanRef as BooleanRef nocode#}
{#pointer CFBundleRef as BundleRef nocode#}
{#pointer CFDataRef as DataRef nocode#}
{#pointer CFDictionaryRef as DictionaryRef nocode#}
{#pointer CFErrorRef as ErrorRef nocode#}
{#pointer CFNotificationCenterRef as NotificationCenterRef nocode#}
{#pointer CFNumberRef as NumberRef nocode#}
{#pointer CFPropertyListRef as PlistRef nocode#}
{#pointer CFRunLoopRef as RunLoopRef nocode#}
{#pointer CFStringRef as StringRef nocode#}
{#pointer CFURLRef as URLRef nocode#}

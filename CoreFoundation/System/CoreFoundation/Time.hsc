module System.CoreFoundation.Time(
                AbsoluteTime,
                getCurrent,
                ) where

import System.CoreFoundation.Internal.TH

#include <CoreFoundation/CoreFoundation.h>

type AbsoluteTime = #type CFAbsoluteTime

unsafeForeignImport "CFAbsoluteTimeGetCurrent" [t| IO AbsoluteTime |]

getCurrent :: IO AbsoluteTime
getCurrent = c_CFAbsoluteTimeGetCurrent

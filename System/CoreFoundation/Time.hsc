module System.CoreFoundation.Time where

import System.CoreFoundation.TH

#include <CoreFoundation/CoreFoundation.h>

type AbsoluteTime = #type CFAbsoluteTime

unsafeForeignImport "CFAbsoluteTimeGetCurrent" [t| IO AbsoluteTime |]

getCurrent :: IO AbsoluteTime
getCurrent = c_CFAbsoluteTimeGetCurrent

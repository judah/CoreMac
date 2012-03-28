-- | CoreFoundation @CFDate@. See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFDateRef/Reference/reference.html>
module System.CoreFoundation.Date(
  -- * Types
  Date,
  DateRef,
  -- * Conversion
  toUTCTime,
  fromUTCTime,
  ) where

#include <CoreFoundation/CFDate.h>

import System.IO.Unsafe (unsafePerformIO)
import Data.Time
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import Foreign.C
import Foreign.Ptr

declareCFType "Date"
{#pointer CFDateRef as DateRef nocode#}

-- | Convert to a 'UTCTime'
toUTCTime :: Date -> UTCTime
toUTCTime o = getAbsTime o `addUTCTime` appleEpoch

{#fun pure unsafe CFDateGetAbsoluteTime as getAbsTime
  { withObject* `Date' } -> `NominalDiffTime' realToFrac#}

-- | Convert from a 'UTCTime'
fromUTCTime :: UTCTime -> Date
fromUTCTime t = unsafePerformIO $ getOwned $ dateCreate (t `diffUTCTime` appleEpoch)

{#fun unsafe CFDateCreate as dateCreate
  { withDefaultAllocator- `AllocatorPtr', realToFrac `NominalDiffTime' } -> `DateRef' id#}

appleEpoch :: UTCTime
appleEpoch = 
  UTCTime{
    utctDay = fromGregorian 2001 1 1,
    utctDayTime = 0
    }

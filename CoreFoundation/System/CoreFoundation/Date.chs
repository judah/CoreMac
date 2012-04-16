-- | CoreFoundation @CFDate@ and @CFAbsoluteTime@. See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFDateRef/Reference/reference.html>
module System.CoreFoundation.Date(
  -- * Types
  -- $dateversusabs
  -- ** Date
  Date,
  DateRef,
  CFDate,
  -- ** AbsoluteTime
  AbsoluteTime(..),
  -- * Conversions
  -- ** Date/AbsoluteTime
  date2abs,
  abs2date,
  -- ** Date/UTCTime
  date2utc,
  utc2date,
  -- ** AbsoluteTime/UTCTime
  abs2utc,
  utc2abs,
  -- * System routines
  getCurrentTime,
  -- * Epoch
  appleEpoch,
  ) where

#include <CoreFoundation/CFDate.h>

import System.IO.Unsafe (unsafePerformIO)
import Data.Time hiding (getCurrentTime)
import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
import Foreign.C
import Foreign.Ptr
import Data.Typeable
import Control.DeepSeq

{- $dateversusabs
'AbsoluteTime' and 'Date' are similar types: 'AbsoluteTime' is a plain C value type
representing time in seconds since the 'appleEpoch'; 'Date' is CoreFoundation 
object which wraps 'AbsoluteTime', and may be put in CoreFoundation containers.
-}

declareCFType "Date"
{#pointer CFDateRef as DateRef nocode#}

deriving instance Typeable Date
instance Show Date where
  show = show . date2utc
instance Eq Date where
  a == b = date2abs a == date2abs b
instance Ord Date where
  compare a b = compare (date2abs a) (date2abs b)
instance NFData Date

-- | Absolute time representing seconds since the 'appleEpoch'.
newtype AbsoluteTime = AbsoluteTime { unAbsoluteTime :: {#type CFAbsoluteTime #} }
  deriving (Eq, Ord, Real, Fractional, Typeable, Num)

instance NFData AbsoluteTime

instance Show AbsoluteTime where
  show = show . abs2utc

----- Conversions
-- Date/AbsoluteTime
{#fun pure unsafe CFDateGetAbsoluteTime as date2abs
  { withObject* `Date' } -> `AbsoluteTime' AbsoluteTime#}

abs2date :: AbsoluteTime -> Date
abs2date t = unsafePerformIO $ getOwned $ dateCreate t

{#fun unsafe CFDateCreate as dateCreate
  { withDefaultAllocator- `AllocatorPtr', unAbsoluteTime `AbsoluteTime' } -> `DateRef' id#}

-- AbsoluteTime/UTCTime
abs2utc :: AbsoluteTime -> UTCTime
abs2utc t = realToFrac t `addUTCTime` appleEpoch

utc2abs :: UTCTime -> AbsoluteTime
utc2abs t = realToFrac (t `diffUTCTime` appleEpoch)

-- Date/UTCTime
date2utc :: Date -> UTCTime
date2utc = abs2utc . date2abs

utc2date :: UTCTime -> Date
utc2date = abs2date . utc2abs

------ System calls
-- | Get the current time
{#fun unsafe CFAbsoluteTimeGetCurrent as getCurrentTime
    {} -> `AbsoluteTime' AbsoluteTime #}

----- Epoch
-- | Midnight, January 1, 2001.
appleEpoch :: UTCTime
appleEpoch = 
  UTCTime{
    utctDay = fromGregorian 2001 1 1,
    utctDayTime = 0
    }




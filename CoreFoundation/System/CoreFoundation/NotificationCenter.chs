-- | Notification centers.  This is *not* toll-free bridged with @NSNotificationCenter@.
module System.CoreFoundation.NotificationCenter (
                            NotificationCenter,
                            NotificationCenterRef,
                            getLocalCenter,
                            postNotification,
                                ) where

import Foreign.Ptr
import Foreign.C
import Prelude hiding (String)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable
import Control.DeepSeq

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
{#import System.CoreFoundation.String#}
{#import System.CoreFoundation.Dictionary#}

#include <CoreFoundation/CoreFoundation.h>

declareCFType "NotificationCenter"
{#pointer CFNotificationCenterRef as NotificationCenterRef nocode#}

{#fun CFNotificationCenterGetLocalCenter as getLocalCenter
    { } -> `NotificationCenter' getAndRetain* #}

{#fun CFNotificationCenterPostNotification as postNotification
    { withObject* `NotificationCenter'
    , withObject* `String'
    , id `Ptr ()'
    , withMaybeObject* `Maybe (Dictionary k v)'
    , id `CBoolean'
    } -> `()' #}

deriving instance Typeable NotificationCenter
instance NFData NotificationCenter
-- | 'Eq' defined on the underlying pointer
deriving instance Eq NotificationCenter
-- | 'Ord' defined on the underlying pointer
deriving instance Ord NotificationCenter
-- | 'Show' defined using 'getObjectDescription'
instance Show NotificationCenter where
  show = unsafePerformIO . getObjectDescription

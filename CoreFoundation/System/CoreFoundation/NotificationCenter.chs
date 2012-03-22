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
    , withMaybeObject* `Maybe Dictionary'
    , id `CBoolean'
    } -> `()' #}

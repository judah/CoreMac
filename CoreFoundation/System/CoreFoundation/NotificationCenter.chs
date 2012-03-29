{- | Notification centers.  This is *not* toll-free bridged with @NSNotificationCenter@.

A 'NotificationCentre' object provides the means by which you can send a message, or notification, to any number of recipients, or observers, without having to know anything about the recipients. A notification message consists of a notification name (a 'String'), a pointer value that identifies the object posting the notification, and an optional dictionary that contains additional information about the particular notification. See <https://developer.apple.com/library/mac/#documentation/CoreFoundation/Reference/CFNotificationCenterRef/Reference/reference.html> for further details.
-}

module System.CoreFoundation.NotificationCenter (
                            NotificationCenter,
                            NotificationCenterRef,
                            getLocalCenter,
                            postNotification,
                                ) where

import Foreign.Marshal
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

{- | 
Returns the application's local notification center. An application has only one local notification center, so this function returns the same value each time it is called.
-}
{#fun CFNotificationCenterGetLocalCenter as getLocalCenter
    { } -> `NotificationCenter' getAndRetain* #}

-- | Posts a notification for an object. For more details, see <https://developer.apple.com/library/mac/documentation/CoreFoundation/Reference/CFNotificationCenterRef/Reference/reference.html#//apple_ref/c/func/CFNotificationCenterPostNotification>
postNotification ::
    NotificationCenter 
    -- ^ The notification center to post the notification.

 -> String 
    -- ^ The name of the notification to post.

 -> Ptr () 
    -- ^ The object posting the notification.

 -> Maybe (Dictionary k v) 
    -- ^ A dictionary passed to observers. You populate
    -- this dictionary with additional information describing
    -- the notification. For distributed notifications, the
    -- dictionary must contain only property list objects.

  -> Bool 
    -- ^ If True, the notification is delivered to all observers 
    -- immediately, even if some observers are in suspended 
    -- (background) applications and they requested different 
    -- suspension behavior when registering for the notification. 
    -- If False, each observer’s requested suspension behavior is respected.

  -> IO ()
postNotification = cfPostNotification

{#fun CFNotificationCenterPostNotification as cfPostNotification
    { withObject* `NotificationCenter'
    , withObject* `String'
    , id `Ptr ()'
    , withMaybeObject* `Maybe (Dictionary k v)'
    , `Bool'
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

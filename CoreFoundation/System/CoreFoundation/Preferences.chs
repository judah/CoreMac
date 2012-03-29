{- |
Core Foundation provides a simple, standard way to manage user (and application) preferences. Core Foundation stores preferences as key-value pairs that are assigned a scope using a combination of user name, application ID, and host (computer) names. This makes it possible to save and retrieve preferences that apply to different classes of users. Core Foundation preferences is useful to all applications that support user preferences. Note that modification of some preferences domains (those not belonging to the \"Current User\") requires root privileges (or Admin privileges prior to Mac OS X v10.6)-see Authorization Services Programming Guide (<https://developer.apple.com/library/mac/#documentation/Security/Conceptual/authorization_concepts/01introduction/introduction.html>)
for information on how to gain suitable privileges.
-}
module System.CoreFoundation.Preferences(
  -- * Types
  Key,
  SuiteID,
  AppID,
  anyApp,
  currentApp,
  HostID,
  anyHost,
  currentHost,
  UserID,
  anyUser,
  currentUser,
  -- * Getting
  getAppValue,
  getKeyList,
  getMultiple,
  getValue,
  -- * Setting
  setAppValue,
  setMultiple,
  setValue,
  -- * Synchronizing
  SyncFailed(..),
  appSync,
  sync,
  -- * Suite preferences
  addSuiteToApp,
  removeSuiteFromApp,
  -- * Misc
  appValueIsForced,
  getAppList,
  ) where

import Prelude hiding(String)
import Control.Applicative
import Control.Exception (Exception)
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr

{#import System.CoreFoundation.Array#}
{#import System.CoreFoundation.String#}
{#import System.CoreFoundation.PropertyList#}
{#import System.CoreFoundation.Dictionary#}
import System.CoreFoundation.Foreign

#include <CoreFoundation/CFPreferences.h>

-- | Keys. These can be constructed using the OverloadedStrings
-- language extension.
type Key = String

-- | ID of a suite, for example @com.apple.iApps@.
type SuiteID = String

-- | Application ID. Takes the form of a java package name, @com.foosoft@, or one of the constants 'anyApp', 'currentApp'.
type AppID = String

importCFString "kCFPreferencesAnyApplication"
importCFString "kCFPreferencesCurrentApplication"
importCFString "kCFPreferencesAnyHost"
importCFString "kCFPreferencesCurrentHost"
importCFString "kCFPreferencesAnyUser"
importCFString "kCFPreferencesCurrentUser"

-- | Matches any application. This may be passed to functions which \"search\", such as 'getAppValue', 'getValue', 'getKeyList', etc. However, this may not be passed to functions which put a value in a specific location, such as 'setAppValue', 'syncApp'.
anyApp :: AppID
anyApp = kCFPreferencesAnyApplication


-- | Specifies the current application
currentApp :: AppID
currentApp = kCFPreferencesCurrentApplication

-- | Host ID. User-provided, or see the constants 'anyHost', 'currentHost'
type HostID = String

-- | When passed to functions which \"search\", such as 'getValue', 'getKeyList', etc, this allows the search to match any host. When passed to \"setting\" functions such as 'setValue', 'sync', it sets the value for all hosts.
anyHost :: HostID
anyHost = kCFPreferencesAnyHost

-- | Current host
currentHost :: HostID
currentHost = kCFPreferencesCurrentHost

-- | User ID. User-provided, or see the constants 'anyUser', 'currentUser'
type UserID = String

-- | When passed to functions which \"search\", such as 'getValue', 'getKeyList', etc, this allows the search to match any user. When passed to \"setting\" functions such as 'setValue', 'sync', it sets the value for all users.
anyUser :: UserID
anyUser = kCFPreferencesAnyUser

-- | Current user
currentUser :: UserID
currentUser = kCFPreferencesCurrentUser

------------------------- Getting ----------------------------
{- |
Obtains a preference value for the specified key and application. Wraps CFPreferencesCopyAppValue.
-}
getAppValue :: Key -> AppID -> IO (Maybe Plist)
getAppValue key app = maybeGetOwned $ cfGetAppValue key app

{#fun CFPreferencesCopyAppValue as cfGetAppValue 
   { withObject* `Key', withObject* `AppID'} -> `PlistRef' id#}

{- |
Constructs and returns the list of all keys set in the specified domain. Wraps CFPreferencesCopyKeyList
-}
getKeyList :: AppID -> UserID -> HostID -> IO (Array Key)
getKeyList app user host = getOwnedArray $ cfGetKeyList app user host

{#fun CFPreferencesCopyKeyList as cfGetKeyList
   { withObject* `AppID', withObject* `UserID', withObject* `HostID' } -> `ArrayRef' id#}

{- |
Returns a dictionary containing preference values for multiple keys. If no values were located, returns an empty dictionary. 
-}
getMultiple :: Array Key -> AppID -> UserID -> HostID -> IO (Dictionary Key Plist)
getMultiple keys app user host = getOwned $ cfGetMultiple keys app user host

{#fun CFPreferencesCopyMultiple as cfGetMultiple 
   { withObject* `Array Key', withObject* `AppID', withObject* `UserID', withObject* `HostID' } -> `DictionaryRef' id#}

{- |
Returns a preference value for a given domain.

This function is the primitive get mechanism for the higher level preference function 'getAppValue'. Unlike the high-level function, 'getValue' searches only the exact domain specified. Do not use this function directly unless you have a need. Do not use arbitrary user and host names, instead pass the pre-defined domain qualifier constants (i.e. 'currentUser', 'anyUser', 'currentHost', 'anyHost').
-}
getValue :: Key -> AppID -> UserID -> HostID -> IO (Maybe Plist)
getValue key app user host = maybeGetOwned $ cfGetValue key app user host
  
{#fun CFPreferencesCopyValue as cfGetValue
   { withObject* `Key', withObject* `AppID', withObject* `UserID', withObject* `HostID' } -> `PlistRef' id#}

-------------------------- Setting ---------------------------
{- |
Adds, modifies, or removes a preference.

New preference values are stored in the standard application preference location, <~/Library/Preferences/>. When called with 'currentApp', modifications are performed in the preference domain \"Current User, Current Application, Any Host.\" If you need to create preferences in some other domain, use the low-level function 'setValue'.

You must call the 'appSync' function in order for your changes to be saved to permanent storage.

Wraps @CFPreferencesSetAppValue@.
-}
{#fun CFPreferencesSetAppValue as setAppValue
  { withObject* `Key', withMaybeObject* `Maybe Plist', withObject* `AppID' } -> `()' #}

{- |
Convenience function that allows you to set and remove multiple preference values.

Behavior is undefined if a key is in both keysToSet and keysToRemove.

Wraps @CFPreferencesSetMultiple@.
-}
{#fun CFPreferencesSetMultiple as setMultiple
  { withObject* `Dictionary Key Plist',
    withObject* `Array Key',
    withObject* `AppID',
    withObject* `UserID',
    withObject* `HostID' }
  -> `()' #}

{- |
Adds, modifies, or removes a preference value for the specified domain.

This function is the primitive set mechanism for the higher level preference function 'setAppValue'. Only the exact domain specified is modified. Do not use this function directly unless you have a specific need. Do not use arbitrary user and host names, instead pass the pre-defined constants.

You must call the 'sync' function in order for your changes to be saved to permanent storage. Note that you can only save preferences for \"Any User\" if you have root privileges (or Admin privileges prior to Mac OS X v10.6).
-}
{#fun CFPreferencesSetValue as setValue
  { withObject* `Key',
    withMaybeObject* `Maybe Plist',
    withObject* `AppID',
    withObject* `UserID',
    withObject* `HostID' }
  -> `()' #}

------------------------- Synchronizing ---------------
-- | Exception thrown when 'appSync' or 'sync' fail.
data SyncFailed = SyncFailed
  deriving(Typeable)
instance Show SyncFailed where
  show _ = "Syncing Apple preferences failed"
instance Exception SyncFailed

{- |
Writes to permanent storage all pending changes to the preference data for the application, and reads the latest preference data from permanent storage.

Calling the function 'setAppValue' is not in itself sufficient for storing preferences. The 'appSync' function writes to permanent storage all pending preference changes for the application. Typically you would call this function after multiple calls to 'setAppValue'. Conversely, preference data is cached after it is first read. Changes made externally are not automatically incorporated. The 'appSync' function reads the latest preferences from permanent storage.

Throws 'SyncFailed' if synchronization failed.
-}
{#fun CFPreferencesAppSynchronize as appSync
  { withObject* `AppID' } -> `()' 'ensureTrue SyncFailed'* #}

{- |
For the specified domain, writes all pending changes to preference data to permanent storage, and reads latest preference data from permanent storage.

This function is the primitive synchronize mechanism for the higher level preference function 'appSync'; it writes updated preferences to permanent storage, and reads the latest preferences from permanent storage. Only the exact domain specified is modified. Note that to modify \"Any User\" preferences requires root privileges (or Admin privileges prior to Mac OS X v10.6) - see <https://developer.apple.com/library/mac/#documentation/Security/Conceptual/authorization_concepts/01introduction/introduction.html>
-}
{#fun CFPreferencesSynchronize as sync
  { withObject* `AppID', withObject* `UserID', withObject* `HostID' } -> `()' 'ensureTrue SyncFailed'* #}

---------------------- Suite preferences -----------
{- |
Adds suite preferences to an application’s preference search chain.

Suite preferences allow you to maintain a set of preferences that are common to all applications in the suite. When a suite is added to an application’s search chain, all of the domains pertaining to that suite are inserted into the chain. Suite preferences are added between the \"Current Application\" domains and the \"Any Application\" domains. If you add multiple suite preferences to one application, the order of the suites in the search chain is non-deterministic. You can override a suite preference for a given application by defining the same preference key in the application specific preferences.

Wraps @CFPreferencesAddSuitePreferencesToApp@.
-}
{#fun CFPreferencesAddSuitePreferencesToApp as addSuiteToApp
  { withObject* `AppID', withObject* `SuiteID' } -> `()' #}

{- |
Removes suite preferences from an application’s search chain.
-}
{#fun CFPreferencesRemoveSuitePreferencesFromApp as removeSuiteFromApp
  { withObject* `AppID', withObject* `SuiteID' } -> `()' #}

----------------------- Misc ------------------
{- |
Determines whether or not a given key has been imposed on the user.

In cases where machines and/or users are under some kind of management, you should use this function to determine whether or not to disable UI elements corresponding to those preference keys.
-}
{#fun CFPreferencesAppValueIsForced as appValueIsForced
  { withObject* `Key', withObject* `AppID' } -> `Bool' '(/=0)' #}

{- |
Constructs and returns the list of all applications that have preferences in the scope of the specified user and host.
-}
getAppList :: UserID -> HostID -> IO (Array AppID)
getAppList user host = getOwnedArray $ cfGetAppList user host

{#fun CFPreferencesCopyApplicationList as cfGetAppList
  { withObject* `UserID', withObject* `HostID' } -> `ArrayRef' id #}




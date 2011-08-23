module System.CoreFoundation.Preferences(
                -- * Getting preference values
                getPref,
                getPrefWithDefault,
                -- * Synchronizing preferences
                synchronizePrefs,
                ) where

import Foreign
import Foreign.C
import Data.Maybe (fromMaybe)

import System.CoreFoundation.Base
import System.CoreFoundation.String
import Prelude hiding (String)

#include <CoreFoundation/CoreFoundation.h>

importCFString "kCFPreferencesCurrentApplication"
importCFString "kCFPreferencesCurrentUser"
importCFString "kCFPreferencesCurrentHost"

-- For now, a very basic API: retrieving preferences for the current application.

{#fun CFPreferencesCopyAppValue as getPrefDyn
    { withObject* `String'
    , 'withObject kCFPreferencesCurrentApplication'- `String'
    } -> `Maybe DynObj' maybeGetOwned* #}


-- The high-level thingy:

getPref :: Object a => String -> IO (Maybe a)
getPref = fmap (>>= castObject) . getPrefDyn

getPrefWithDefault :: Object a => a -> String -> IO a
getPrefWithDefault x = fmap (fromMaybe x) . getPref

{#fun CFPreferencesSynchronize as synchronizePrefs
    { 'withObject kCFPreferencesCurrentApplication'- `String'
    , 'withObject kCFPreferencesCurrentUser'- `String'
    , 'withObject kCFPreferencesCurrentHost'- `String'
    } -> `Bool' '(/=0)' #}

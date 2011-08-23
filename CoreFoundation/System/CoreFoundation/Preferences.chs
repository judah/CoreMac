module System.CoreFoundation.Preferences(
                getPref,
                getPrefWithDefault,
                ) where

import Foreign
import Foreign.C
import Data.Maybe (fromMaybe)

import System.CoreFoundation.Base
import System.CoreFoundation.String
import Prelude hiding (String)

#include <CoreFoundation/CoreFoundation.h>

importCFStringAs "kCFPreferencesCurrentApplication" "preferencesCurrentApplication"

-- For now, a very basic API: retrieving preferences for the current application.

-- TODO: this might return NULL
{#fun CFPreferencesCopyAppValue as getPrefAppValue
    { withObject* `String'
    , withObject* `String'
    } -> `Maybe DynObj' maybeGetOwned* #}


-- The high-level thingy:

getPref :: Object a => String -> IO (Maybe a)
getPref s = fmap (>>= castObject) $ getPrefAppValue s preferencesCurrentApplication

getPrefWithDefault :: Object a => a -> String -> IO a
getPrefWithDefault x = fmap (fromMaybe x) . getPref

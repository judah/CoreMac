module System.CoreFoundation.Preferences(
                -- * Getting preference values
                Preference(..),
                getPref,
                getPrefWithDefault,
                -- * Synchronizing preferences
                synchronizePrefs,
                ) where

import Foreign
import Foreign.C
import Data.Maybe (fromMaybe)

import System.CoreFoundation.Base
import System.CoreFoundation.Array
import System.CoreFoundation.Data
import System.CoreFoundation.Dictionary
import System.CoreFoundation.Number
import System.CoreFoundation.String
import Prelude hiding (String)
import qualified Prelude

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
-- Actually, this should be plist...
class Preference a where
    toPreference :: DynObj -> IO (Maybe a)

instance Preference Array where
    toPreference = return . castObject

instance Preference Dictionary where
    toPreference = return . castObject

instance Preference String where
    toPreference = return . castObject

instance Preference Data where
    toPreference = return . castObject

instance Preference Number where
    toPreference = return . castObject

-- TODO: CFDate and CFBoolean also

instance Preference Prelude.String where
    toPreference p = toPreference p `thenMaybe` (fmap Just . getChars)

instance Preference Int8 where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference Int16 where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference Int32 where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CChar where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CShort where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CInt where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CLong where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CLLong where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CFloat where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference CDouble where
    toPreference = fmap (fmap numberValue) . toPreference

instance Preference Int where
    toPreference = fmap (fmap numberValue) . toPreference

thenMaybe :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
thenMaybe f g = f >>= \mx -> case mx of
                                Nothing -> return Nothing
                                Just x -> g x

getPref :: Preference a => String -> IO (Maybe a)
getPref s = getPrefDyn s `thenMaybe` toPreference

getPrefWithDefault :: Preference a => a -> String -> IO a
getPrefWithDefault x = fmap (fromMaybe x) . getPref

{#fun CFPreferencesSynchronize as synchronizePrefs
    { 'withObject kCFPreferencesCurrentApplication'- `String'
    , 'withObject kCFPreferencesCurrentUser'- `String'
    , 'withObject kCFPreferencesCurrentHost'- `String'
    } -> `Bool' '(/=0)' #}


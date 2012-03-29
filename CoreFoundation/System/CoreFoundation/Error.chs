-- | Interface to CoreFoundation's @CFError@ C type.
-- It is toll-free bridged with the @NSData@ type.
module System.CoreFoundation.Error (
            -- * The Error type
            Error,
            ErrorRef,
            cfError,
            -- * Error properties
            errorDescription,
            errorFailureReason,
            errorRecoverySuggestion,
            userInfo,
            errorDomain,
            errorCode,
            -- * Error domains
            domainPOSIX,
            domainOSStatus,
            domainMach,
            domainCocoa,
            -- * User info keys
            localizedDescriptionKey,
            localizedFailureReasonKey,
            localizedRecoverySuggestionKey,
            descriptionKey,
            underlyingErrorKey,
            ) where

import Foreign.Ptr
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (String)

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
{#import System.CoreFoundation.Dictionary#}
{#import System.CoreFoundation.String#} as CF
import System.CoreFoundation.Internal.TH
import Data.Typeable
import Control.DeepSeq

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Error"
{#pointer CFErrorRef as ErrorRef nocode#}

importCFStringAs "kCFErrorLocalizedDescriptionKey" "localizedDescriptionKey"
importCFStringAs "kCFErrorLocalizedFailureReasonKey" "localizedFailureReasonKey"
importCFStringAs "kCFErrorLocalizedRecoverySuggestionKey" "localizedRecoverySuggestionKey"
importCFStringAs "kCFErrorDescriptionKey" "descriptionKey"
importCFStringAs "kCFErrorUnderlyingErrorKey" "underlyingErrorKey"

importCFStringAs "kCFErrorDomainPOSIX" "domainPOSIX"
importCFStringAs "kCFErrorDomainOSStatus" "domainOSStatus"
importCFStringAs "kCFErrorDomainMach" "domainMach"
importCFStringAs "kCFErrorDomainCocoa" "domainCocoa"

{#fun CFErrorCreate as newError
    { withDefaultAllocator- `AllocatorPtr'
    ,  withObject* `String'
    , `Int'
    , maybeWithObject* `Maybe (Dictionary k v)'
    } -> `ErrorRef' id #}

cfError :: String -- ^ The error domain.
        -> Int -- ^ The error code.
        -> Maybe (Dictionary k v) -- ^ User info.
        -> Error
cfError domain code userInfo = unsafePerformIO $ getOwned
            $ newError domain code userInfo

maybeWithObject Nothing = ($ nullPtr)
maybeWithObject (Just o) = withObject o

{#fun pure CFErrorCopyDescription as errorDescription
    { withObject* `Error' } -> `String' getAndRetain* #}
-- The docs say that the CFErrorCopyDescription will never return NULL.

{#fun pure CFErrorCopyFailureReason as errorFailureReason
    { withObject* `Error' } -> `Maybe String' maybeGetAndRetain* #}

{#fun pure CFErrorCopyRecoverySuggestion as errorRecoverySuggestion
    { withObject* `Error' } -> `Maybe String' maybeGetAndRetain* #}

{#fun pure CFErrorCopyUserInfo as userInfo
    { withObject* `Error' } -> `Maybe (Dictionary k v)' maybeGetAndRetain* #}

{#fun pure CFErrorGetDomain as errorDomain
    { withObject* `Error' } -> `String' getAndRetain* #}

{#fun pure CFErrorGetCode as errorCode
    { withObject* `Error' } -> `CFIndex' id #} 

toPair :: Error -> (String, CFIndex)
toPair err = (errorDomain err, errorCode err)

deriving instance Typeable Error
instance NFData Error
instance Eq Error where
  a == b = toPair a == toPair b
instance Ord Error where
  compare a b = compare (toPair a) (toPair b)
instance Show Error where
  show = getChars . errorDescription

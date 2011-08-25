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

import Foreign
import Foreign.C

import Prelude hiding (String)

import System.CoreFoundation.Base
import System.CoreFoundation.Dictionary
import System.CoreFoundation.String as CF
import System.CoreFoundation.Internal.TH

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Error"

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
    , maybeWithObject* `Maybe Dictionary'
    } -> `Error' getOwned* #}

cfError :: String -- ^ The error domain.
        -> Int -- ^ The error code.
        -> Maybe Dictionary -- ^ User info.
        -> Error
cfError domain code userInfo = unsafePerformIO
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
    { withObject* `Error' } -> `Maybe Dictionary' maybeGetAndRetain* #}

{#fun pure CFErrorGetDomain as errorDomain
    { withObject* `Error' } -> `String' getAndRetain* #}

{#fun pure CFErrorGetCode as errorCode
    { withObject* `Error' } -> `CFIndex' id #} 



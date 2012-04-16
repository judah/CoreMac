-- | Interface to CoreFoundation's @CFError@ C type.
-- It is toll-free bridged with the @NSData@ type.
module System.CoreFoundation.Error (
            -- * The Error type
            -- $error
            Error,
            ErrorRef,
            CFError,
            mkError,
            ErrorCode,
            -- * Error properties
            errorDescription,
            errorFailureReason,
            errorRecoverySuggestion,
            userInfo,
            errorDomain,
            errorCode,
            -- * Error domains
            ErrorDomain,
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
import Control.Exception

#include <CoreFoundation/CoreFoundation.h>

declareCFType "Error"
{#pointer CFErrorRef as ErrorRef nocode#}

{- $error
An 'Error' object encapsulates rich and extensible error information than is possible using only an error code or error string. The core attributes of an 'Error' object are an error domain (represented by a string), a domain-specific error code and a user info dictionary containing application specific information. Errors are required to have a domain and an error code within that domain. The optional 'userInfo' dictionary may provide additional information that might be useful for the interpretation and reporting of the error. This dictionary can even contain an \"underlying\" error, which is wrapped as an error bubbles up through various layers. See <https://developer.apple.com/library/mac/documentation/CoreFoundation/Reference/CFErrorRef/Reference/reference.html#//apple_ref/doc/uid/TP40004341-CH1-DontLinkElementID_13> for further details.

The 'Error' type is an instance of 'Exception', so it may be thrown using 'throw'.
-}

importCFStringAs "kCFErrorLocalizedDescriptionKey" "localizedDescriptionKey"
importCFStringAs "kCFErrorLocalizedFailureReasonKey" "localizedFailureReasonKey"
importCFStringAs "kCFErrorLocalizedRecoverySuggestionKey" "localizedRecoverySuggestionKey"
importCFStringAs "kCFErrorDescriptionKey" "descriptionKey"
importCFStringAs "kCFErrorUnderlyingErrorKey" "underlyingErrorKey"

importCFStringAs "kCFErrorDomainPOSIX" "domainPOSIX"
importCFStringAs "kCFErrorDomainOSStatus" "domainOSStatus"
importCFStringAs "kCFErrorDomainMach" "domainMach"
importCFStringAs "kCFErrorDomainCocoa" "domainCocoa"

-- | Error domains
type ErrorDomain = String
domainPOSIX, domainOSStatus, domainMach, domainCocoa :: ErrorDomain

-- | Error codes
type ErrorCode = CFIndex

{#fun CFErrorCreate as newError
    { withDefaultAllocator- `AllocatorPtr'
    ,  withObject* `String'
    , id `ErrorCode'
    , maybeWithObject* `Maybe (Dictionary DynObj DynObj)'
    } -> `ErrorRef' id #}

-- | Construct a new error
mkError :: ErrorDomain -- ^ The error domain.
        -> ErrorCode -- ^ The error code.
        -> Maybe (Dictionary DynObj DynObj) -- ^ User info.
        -> Error
mkError domain code userInfo = unsafePerformIO $ getOwned
            $ newError domain code userInfo

maybeWithObject Nothing = ($ nullPtr)
maybeWithObject (Just o) = withObject o

{- |
Returns a human-presentable description for a given error. This is a complete sentence or two which says what failed and why it failed. The structure of the description depends on the details provided in the user info dictionary. See <https://developer.apple.com/library/mac/documentation/CoreFoundation/Reference/CFErrorRef/Reference/reference.html#//apple_ref/c/func/CFErrorCopyDescription> for the precise rules used.
-}
{#fun pure CFErrorCopyDescription as errorDescription
    { withObject* `Error' } -> `String' getAndRetain* #}
-- The docs say that the CFErrorCopyDescription will never return NULL.

{- |
Returns a human-presentable failure reason for a given error. See <https://developer.apple.com/library/mac/documentation/CoreFoundation/Reference/CFErrorRef/Reference/reference.html#//apple_ref/c/func/CFErrorCopyFailureReason> for details.

-}
{#fun pure CFErrorCopyFailureReason as errorFailureReason
    { withObject* `Error' } -> `Maybe String' maybeGetAndRetain* #}

{- |
Returns a human presentable recovery suggestion for a given error. See <https://developer.apple.com/library/mac/documentation/CoreFoundation/Reference/CFErrorRef/Reference/reference.html#//apple_ref/c/func/CFErrorCopyRecoverySuggestion> for details.
-}
{#fun pure CFErrorCopyRecoverySuggestion as errorRecoverySuggestion
    { withObject* `Error' } -> `Maybe String' maybeGetAndRetain* #}

{- |
Returns the user info dictionary.
-}
{#fun pure CFErrorCopyUserInfo as userInfo
    { withObject* `Error' } -> `Maybe (Dictionary DynObj DynObj)' maybeGetAndRetain* #}

{- |
Returns the error domain.
-}
{#fun pure CFErrorGetDomain as errorDomain
    { withObject* `Error' } -> `ErrorDomain' getAndRetain* #}

{- |
Returns the error code.
-}
{#fun pure CFErrorGetCode as errorCode
    { withObject* `Error' } -> `ErrorCode' id #} 

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
instance Exception Error

module System.CoreFoundation(
            module System.CoreFoundation.Base,
            module System.CoreFoundation.Array,
            module System.CoreFoundation.Array.Mutable,
            module System.CoreFoundation.Bundle,
            module System.CoreFoundation.Data,
            module System.CoreFoundation.Error,
            module System.CoreFoundation.Dictionary,
            module System.CoreFoundation.NotificationCenter,
            module System.CoreFoundation.Number,
            module System.CoreFoundation.Preferences,
            module System.CoreFoundation.RunLoop,
            module System.CoreFoundation.String,
            module System.CoreFoundation.Time,
            module System.CoreFoundation.URL,
            -- $c2hsdecls
            ) where

import System.CoreFoundation.Base
import System.CoreFoundation.Array
import System.CoreFoundation.Array.Mutable
import System.CoreFoundation.Bundle
import System.CoreFoundation.Data
import System.CoreFoundation.Error
import System.CoreFoundation.Dictionary
import System.CoreFoundation.NotificationCenter
import System.CoreFoundation.Number
import System.CoreFoundation.Preferences
import System.CoreFoundation.RunLoop
import System.CoreFoundation.String
import System.CoreFoundation.Time
import System.CoreFoundation.URL

{- $c2hsdecls
Clients of this library who use c2hs may find the following declarations useful:

>{#pointer CFArrayRef as ArrayRef nocode#}
>{#pointer CFMutableArrayRef as MArrayRef nocode #}
>{#pointer CFTypeRef nocode#}
>{#pointer CFBooleanRef as BooleanRef nocode#}
>{#pointer CFBundleRef as BundleRef nocode#}
>{#pointer CFDataRef as DataRef nocode#}
>{#pointer CFDictionaryRef as DictionaryRef nocode#}
>{#pointer CFErrorRef as ErrorRef nocode#}
>{#pointer CFNotificationCenterRef as NotificationCenterRef nocode#}
>{#pointer CFNumberRef as NumberRef nocode#}
>{#pointer CFPropertyListRef as PlistRef nocode#}
>{#pointer CFRunLoopRef as RunLoopRef nocode#}
>{#pointer CFStringRef as StringRef nocode#}
>{#pointer CFURLRef as URLRef nocode#}

-}
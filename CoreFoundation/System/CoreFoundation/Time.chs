module System.CoreFoundation.Time(
                AbsoluteTime,
                getCurrentTime,
                ) where

import Foreign.C

#include <CoreFoundation/CoreFoundation.h>

type AbsoluteTime = {#type CFAbsoluteTime #}

{#fun CFAbsoluteTimeGetCurrent as getCurrentTime
    {} -> `AbsoluteTime' id #}

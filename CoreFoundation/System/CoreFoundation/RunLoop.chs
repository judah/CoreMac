-- | Core Foundation run loops.  
module System.CoreFoundation.RunLoop(
                    RunLoop,
                    getCurrentRunLoop,
                    getMainRunLoop,
                    queueIO,
                    queueIOAndWait,
                    ) where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C
import Control.Concurrent
import Control.Monad.Fix
import Data.Typeable
import Control.DeepSeq
import System.IO.Unsafe (unsafePerformIO)

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH

#include "runloop.h"
#include <CoreFoundation/CoreFoundation.h>

declareCFType "RunLoop"
{#pointer CFRunLoopRef as RunLoopRef nocode#}

-- | Returns the 'RunLoop' for the current OS thread.  Every OS (i.e., POSIX)
-- thread has exactly one run loop associated with it.
{#fun CFRunLoopGetCurrent as getCurrentRunLoop 
    { } -> `RunLoop' getAndRetain* #}

-- | Returns the 'RunLoop' for the program's main thread.
{#fun CFRunLoopGetMain as getMainRunLoop
    { } -> `RunLoop' getAndRetain* #}

{#fun queueFunctionForRunLoop
    { withObject* `RunLoop'
    , id `Ptr ()'
    } -> `()' #}

{#fun CFRunLoopWakeUp as wakeUpRunLoop
    { withObject* `RunLoop' } -> `()' #}

foreign export ccall runHSAction :: StablePtr (IO ()) -> IO ()

runHSAction d = do
    f <- deRefStablePtr d
    f

-- | Enqueue an action to be performed on the given 'RunLoop'.  This method
-- does not wait for the action to complete.
--
-- This function can only be used in programs compiled with
-- GHC's @-threaded@ option.
queueIO :: RunLoop -> IO a -> IO ()
queueIO r f = do
    d <- mfix $ \d -> newStablePtr $ f >> freeStablePtr d
    queueFunctionForRunLoop r $ castStablePtrToPtr d
    wakeUpRunLoop r

-- | Enqueue an action to be performed on the given 'RunLoop', and wait for it
-- to complete.
-- 
-- This function can only be used in programs compiled with
-- GHC's @-threaded@ option.
queueIOAndWait :: RunLoop -> IO a -> IO a
queueIOAndWait r f = do
    v <- newEmptyMVar
    d <- mfix $ \d -> newStablePtr $ f >>= putMVar v
    queueFunctionForRunLoop r $ castStablePtrToPtr d
    wakeUpRunLoop r
    x <- takeMVar v
    freeStablePtr d
    return x

deriving instance Typeable RunLoop
instance NFData RunLoop
-- | 'Eq' defined on the underlying pointer
deriving instance Eq RunLoop
-- | 'Ord' defined on the underlying pointer
deriving instance Ord RunLoop
-- | 'Show' defined using 'getObjectDescription'
instance Show RunLoop where
  show = unsafePerformIO . getObjectDescription

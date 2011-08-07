module System.NSApplication.Main(nsApplicationMain) where

import Foreign
import Foreign.C

foreign import ccall safe "NSApplicationMain" _NSApplicationMain :: CInt -> Ptr (Ptr CChar) -> IO CInt

-- | Call this in your @main@ function to initialize and run the application.
--
-- This function never returns; instead, it calls the @exit@ function to exit the
-- application and terminate the process.
nsApplicationMain :: IO ()
nsApplicationMain = do
    _NSApplicationMain 0 nullPtr -- NSApplicationMain ignores its arguments.
    return ()

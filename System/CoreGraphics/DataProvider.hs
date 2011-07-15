{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider where

import Foreign
import Foreign.C

import System.CoreFoundation.Base

newtype DataProvider = DataProvider (ForeignPtr ())
type DataProviderRef = Ptr ()

instance CFType DataProvider where
    cftype = DataProvider
    uncftype (DataProvider p) = p


foreign import ccall unsafe "CGDataProviderCreateWithFilename"
    c_CGDataProviderCreateWithFilename :: CString -> IO DataProviderRef

createWithFilename :: FilePath -> IO DataProvider
createWithFilename path = withCString path $ \cstr -> do
    c_CGDataProviderCreateWithFilename cstr >>= retained

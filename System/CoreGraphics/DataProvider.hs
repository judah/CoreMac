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

dataProviderWithFilename :: FilePath -> IO DataProvider
dataProviderWithFilename path = withCString path $ \cstr -> do
    c_CGDataProviderCreateWithFilename cstr >>= retained

{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.TH

declareCFType "DataProvider"

unsafeForeignImport "CGDataProviderCreateWithFilename"
    [t| CString -> IO DataProviderRef |]

dataProviderWithFilename :: FilePath -> IO DataProvider
dataProviderWithFilename path = withCString path $ \cstr -> do
    c_CGDataProviderCreateWithFilename cstr >>= created

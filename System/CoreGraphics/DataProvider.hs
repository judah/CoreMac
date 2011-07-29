{-# LANGUAGE ForeignFunctionInterface #-}
module System.CoreGraphics.DataProvider(
                    DataProvider,
                    DataProviderRef,
                    dataProviderWithFilename,
                    ) where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

declareCFType "DataProvider"

unsafeForeignImport "CGDataProviderCreateWithFilename"
    [t| CString -> IO DataProviderRef |]

dataProviderWithFilename :: FilePath -> IO DataProvider
dataProviderWithFilename path = withCString path $ \cstr -> do
    c_CGDataProviderCreateWithFilename cstr >>= getOwned

-- | Foundation functions relating to user info and file paths
--
-- NOTE: these functions aren't from CoreFoundation, and thus
-- require an autorelease pool.
module System.CoreFoundation.FilePath where

import Foreign
import Foreign.C

import System.CoreFoundation.TH
import System.CoreFoundation.Base
import System.CoreFoundation.String as CF

unsafeForeignImport "NSFullUserName" [t| IO CFStringRef |]
unsafeForeignImport "NSHomeDirectory" [t| IO CFStringRef |]
unsafeForeignImport "NSHomeDirectoryForUser" [t| CFStringRef -> IO CFStringRef |]
unsafeForeignImport "NSTemporaryDirectory" [t| IO CFStringRef |]
unsafeForeignImport "NSUserName" [t| IO CFStringRef |]

fullUserName :: IO CF.String
fullUserName = c_NSFullUserName >>= retained

homeDirectory :: IO CF.String
homeDirectory = c_NSHomeDirectory >>= retained

homeDirectoryForUser :: CF.String -> IO CF.String
homeDirectoryForUser s = cfWith s $ \s' -> c_NSHomeDirectoryForUser s'
                                            >>= retained

temporaryDirectory :: IO CF.String
temporaryDirectory = c_NSTemporaryDirectory >>= retained

userName :: IO CF.String
userName = c_NSUserName >>= retained

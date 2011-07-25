module System.CoreGraphics.Context where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.TH
import System.CoreGraphics.Geometry

declareCFType "Context"

-- TODO:
-- CGContextShowGlyphsAtPoint
-- CGContextFillRect

foreign import ccall unsafe
    c_CGContextFillRect :: ContextRef -> CGFloat -> CGFloat -> CGFloat -> CGFloat -> IO ()

fillRect :: Context -> Rect -> IO ()
fillRect c (Rect p s) = withCF c $ \cp -> c_CGContextFillRect cp (pointX p) (pointY p)
                                                    (sizeWidth s) (sizeHeight s)

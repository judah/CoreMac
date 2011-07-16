module System.CoreGraphics.Context where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreGraphics.Geometry

newtype Context = Context (ForeignPtr ())
type ContextRef = Ptr ()

instance CFType Context where
    cftype = Context
    uncftype (Context p) = p


-- TODO:
-- CGContextShowGlyphsAtPoint
-- CGContextFillRect

foreign import ccall unsafe
    c_CGContextFillRect :: ContextRef -> CGFloat -> CGFloat -> CGFloat -> CGFloat -> IO ()

fillRect :: Context -> Rect -> IO ()
fillRect c (Rect p s) = cfWith c $ \cp -> c_CGContextFillRect cp (pointX p) (pointY p)
                                                    (sizeWidth s) (sizeHeight s)

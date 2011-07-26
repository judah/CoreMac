module System.CoreGraphics.Context where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.TH

import System.CoreGraphics.Geometry
import System.CoreGraphics.Font
import System.CoreGraphics.AffineTransform

declareCFType "Context"

foreign import ccall unsafe
    c_CGContextFillRect :: ContextRef -> Ptr Rect -> IO ()

unsafeForeignImport "CGContextShowGlyphsAtPoint"
    [t| ContextRef -> CGFloat -> CGFloat -> Ptr Glyph -> CSize -> IO () |]

unsafeForeignImport "CGContextSetFont" [t| ContextRef -> FontRef -> IO () |]
unsafeForeignImport "CGContextSetFontSize" [t| ContextRef -> CGFloat -> IO () |] 

foreign import ccall unsafe
    c_CGContextSetTextMatrix :: ContextRef -> Ptr AffineTransform
                                    -> IO ()
-- unsafeForeignImport "CGontextSetTextMatrix"

fillRect :: Context -> Rect -> IO ()
fillRect c r = withCF c $ \cp -> with r $ \rp -> c_CGContextFillRect cp rp

showGlyphsAtPoint :: Context -> Point -> [Glyph] -> IO ()
showGlyphsAtPoint c pt gs = withArrayLen gs $ \gs_n gs_ptr -> 
                            withCF c $ \c_p ->
    c_CGContextShowGlyphsAtPoint c_p (pointX pt) (pointY pt) gs_ptr (toEnum gs_n)

setTextMatrix :: Context -> AffineTransform -> IO ()
setTextMatrix c a = withCF c $ \cp -> with a $ \ap ->
                        c_CGContextSetTextMatrix cp ap

module System.CoreGraphics.Context where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.TH

import System.CoreGraphics.Geometry
import System.CoreGraphics.Font

declareCFType "Context"

-- TODO:
-- CGContextShowGlyphsAtPoint
-- CGContextFillRect

foreign import ccall unsafe
    c_CGContextFillRect :: ContextRef -> CGFloat -> CGFloat -> CGFloat -> CGFloat -> IO ()

unsafeForeignImport "CGContextShowGlyphsAtPoint"
    [t| ContextRef -> CGFloat -> CGFloat -> Ptr Glyph -> CSize -> IO () |]

unsafeForeignImport "CGContextSetFont" [t| ContextRef -> FontRef -> IO () |]
unsafeForeignImport "CGContextSetFontSize" [t| ContextRef -> CGFloat -> IO () |] 
-- unsafeForeignImport "CGontextSetTextMatrix"

fillRect :: Context -> Rect -> IO ()
fillRect c (Rect p s) = withCF c $ \cp -> c_CGContextFillRect cp (pointX p) (pointY p)
                                                    (sizeWidth s) (sizeHeight s)

showGlyphsAtPoint :: Context -> Point -> [Glyph] -> IO ()
showGlyphsAtPoint c pt gs = withArrayLen gs $ \gs_n gs_ptr -> 
                            withCF c $ \c_p ->
    c_CGContextShowGlyphsAtPoint c_p (pointX pt) (pointY pt) gs_ptr (toEnum gs_n)

module System.CoreGraphics.Font where

import Foreign
import Foreign.C
import Control.Monad

import System.CoreFoundation.Base
import System.CoreGraphics.DataProvider

newtype Font = Font (ForeignPtr ())
type FontRef = Ptr ()

instance CFType Font where
    cftype = Font
    uncftype (Font p) = p

foreign import ccall unsafe "CGFontCreateWithDataProvider"
    c_CGFontCreateWithDataProvider :: DataProviderRef -> IO FontRef

fontWithDataProvider :: DataProvider -> IO Font
fontWithDataProvider d = cfWith d $ \dp -> c_CGFontCreateWithDataProvider dp >>= retained

type Glyph = CUShort -- TODO: use hsc2hs.  But need GHC's #5106 to be fixed first.
                    -- (use <ApplicationServices/ApplicationServices.h>)

foreign import ccall unsafe "CGFontGetGlyphAdvances"
    c_CGFontGetGlyphAdvances :: FontRef -> Ptr Glyph -> CSize -> Ptr CInt -> IO CBool

glyphAdvances :: Font -> [Glyph] -> [CInt]
glyphAdvances f glyphs = unsafePerformIO $ do
    cfWith f $ \fp -> do
    withArrayLen glyphs $ \len gp -> do
    allocaArray len $ \sp -> do
    result <- c_CGFontGetGlyphAdvances fp gp (toEnum len) sp
    -- In my tests, if you pass an invalid character (e.g., too high or negative), 
    -- it just returns zero for each of them.
    when (result /= 1) $ error "getGlyphAdvances: couldn't get advance"
    peekArray len sp

foreign import ccall unsafe "CGFontGetUnitsPerEm"
    c_CGFontGetUnitsPerEm :: FontRef -> IO CInt

unitsPerEm :: Font -> CInt
unitsPerEm f = unsafePerformIO $ cfWith f c_CGFontGetUnitsPerEm
    

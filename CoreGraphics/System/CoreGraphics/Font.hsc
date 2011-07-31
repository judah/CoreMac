module System.CoreGraphics.Font(
                    Font,
                    FontRef,
                    fontWithDataProvider,
                    Glyph,
                    glyphAdvances,
                    unitsPerEm,
                    ) where

import Foreign
import Foreign.C
import Control.Monad

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH
import System.CoreGraphics.DataProvider

#include <ApplicationServices/ApplicationServices.h>

declareCFType "Font"

unsafeForeignImport "CGFontCreateWithDataProvider"
    [t| DataProviderRef -> IO FontRef |]

fontWithDataProvider :: DataProvider -> IO Font
fontWithDataProvider d = withCF d $ \dp -> 
            c_CGFontCreateWithDataProvider dp >>= getOwned

type Glyph = #type CGGlyph

unsafeForeignImport "CGFontGetGlyphAdvances"
    [t| FontRef -> Ptr Glyph -> CSize -> Ptr CInt -> IO CBool |]

glyphAdvances :: Font -> [Glyph] -> [CInt]
glyphAdvances f glyphs = unsafePerformIO $ do
    withCF f $ \fp -> do
    withArrayLen glyphs $ \len gp -> do
    allocaArray len $ \sp -> do
    result <- c_CGFontGetGlyphAdvances fp gp (toEnum len) sp
    -- In my tests, if you pass an invalid character (e.g., too high or negative), 
    -- it just returns zero for each of them.
    when (result /= 1) $ error "getGlyphAdvances: couldn't get advance"
    peekArray len sp

unsafeForeignImport "CGFontGetUnitsPerEm" [t| FontRef -> IO CInt |]

unitsPerEm :: Font -> CInt
unitsPerEm f = unsafePerformIO $ withCF f c_CGFontGetUnitsPerEm
    

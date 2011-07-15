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

foreign import ccall "CGFontCreateWithDataProvider"
    c_CGFontCreateWithDataProvider :: DataProviderRef -> IO FontRef

createWithDataProvider :: DataProvider -> IO Font
createWithDataProvider d = cfWith d $ \dp -> c_CGFontCreateWithDataProvider dp >>= retained

type Glyph = CUShort

type CBool = CInt -- ???

foreign import ccall "CGFontGetGlyphAdvances"
    c_CGFontGetGlyphAdvances :: FontRef -> Ptr Glyph -> CSize -> Ptr CInt -> IO CBool

getGlyphAdvances :: Font -> [Glyph] -> IO [CInt]
getGlyphAdvances f glyphs = do
    cfWith f $ \fp -> do
    withArrayLen glyphs $ \len gp -> do
    allocaArray len $ \sp -> do
    result <- c_CGFontGetGlyphAdvances fp gp (toEnum len) sp
    -- In my tests, if you pass an invalid character (e.g., too high or negative), 
    -- it just returns zero for each of them.
    when (result /= 1) $ error "getGlyphAdvances: couldn't get advance"
    peekArray len sp

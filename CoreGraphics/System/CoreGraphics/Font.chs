module System.CoreGraphics.Font(
                    Font,
                    FontRef,
                    newFontWithDataProvider,
                    Glyph,
                    glyphAdvances,
                    unitsPerEm,
                    ) where

import Foreign.Ptr
import Foreign.C
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Array

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
{#import System.CoreGraphics.DataProvider#}

#include <ApplicationServices/ApplicationServices.h>
#include "font.h"

declareCFTypeAs "CGFont" "Font"
{#pointer CGFontRef as FontRef nocode#}

newFontWithDataProvider :: DataProvider -> IO Font
newFontWithDataProvider dp = getOwned $ cgNewFontWithDataProvider dp

{#fun unsafe CGFontCreateWithDataProvider as cgNewFontWithDataProvider
    { withObject* `DataProvider'
    } -> `FontRef' id #}

type Glyph = {#type CGGlyph #}

{#fun pure unsafe CGFontGetUnitsPerEm as unitsPerEm
    { withObject* `Font'
    } -> `Int' #}

{#fun unsafe c_CGFontGetGlyphAdvances
    { withObject* `Font'
    , id `Ptr Glyph'
    , id `CULong'
    , id `Ptr CInt'
    } -> `Bool' '(==1)' #}

glyphAdvances :: Font -> [Glyph] -> [CInt]
glyphAdvances f glyphs = unsafePerformIO $ do
    withArrayLen glyphs $ \len gp -> do
    allocaArray len $ \sp -> do
    result <- c_CGFontGetGlyphAdvances f gp (toEnum len) sp
    -- In my tests, if you pass an invalid character (e.g., too high or negative), 
    -- it just returns zero for each of them.
    when (not result) $ error "getGlyphAdvances: couldn't get advance"
    peekArray len sp


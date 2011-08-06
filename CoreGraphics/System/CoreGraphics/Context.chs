module System.CoreGraphics.Context(
                Context,
                ContextRef,
                fillRect,
                showGlyphsAtPoint,
                setFont,
                setFontSize,
                setTextMatrix,
                ) where

import Foreign
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH

import System.CoreGraphics.Geometry
import System.CoreGraphics.Font
import System.CoreGraphics.AffineTransform

#include "context.h"

declareCFType "Context"

with_ :: Storable a => a -> (Ptr () -> IO c) -> IO c
with_ x f = with x $ f . castPtr

withArrayLen_ :: (Storable a, Enum b) => [a] -> ((Ptr a,b) -> IO c) -> IO c
withArrayLen_ gs f = withArrayLen gs (\n p -> f (castPtr p,toEnum n))

{#fun unsafe c_CGContextFillRect as fillRect
    { withCF* `Context',
      'with_'* `Rect'
    } -> `()' #}

{#fun unsafe CGContextShowGlyphsAtPoint as showGlyphsAtPoint'
    { withCF* `Context',
      id `CGFloat', id `CGFloat',
      'withArrayLen_'* `[Glyph]'&
    } -> `()' #}

showGlyphsAtPoint :: Context -> Point -> [Glyph] -> IO ()
showGlyphsAtPoint c pt gs = showGlyphsAtPoint' c (pointX pt) (pointY pt) gs

{#fun unsafe CGContextSetFont as setFont
    { withCF* `Context',
      withCF* `Font'
    } -> `()' #}

{#fun unsafe CGContextSetFontSize as setFontSize
    { withCF* `Context',
      id `CGFloat'
    } -> `()' #}

{#fun c_CGContextSetTextMatrix as setTextMatrix
    { withCF* `Context'
    , 'with_'* `AffineTransform'
    } -> `()' #}
      


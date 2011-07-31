module System.CoreGraphics.Geometry where

import Foreign.Storable

#include <ApplicationServices/ApplicationServices.h>

type CGFloat = #type CGFloat

data Point = Point { pointX, pointY :: !CGFloat }
                deriving Show

instance Storable Point where
    sizeOf _ = #size CGPoint
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        pointX <- (#peek CGPoint, x) p
        pointY <- (#peek CGPoint, y) p
        return Point {..}
    poke p Point {..} = do
        (#poke CGPoint, x) p pointX
        (#poke CGPoint, y) p pointY

data Size = Size { sizeWidth , sizeHeight :: !CGFloat }
                deriving Show

instance Storable Size where
    sizeOf _ = #size CGSize
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        sizeWidth <- (#peek CGSize, width) p
        sizeHeight <- (#peek CGSize, height) p
        return Size {..}
    poke p Size {..} = do
        (#poke CGSize, width) p sizeWidth
        (#poke CGSize, height) p sizeHeight


data Rect = Rect { rectOrigin :: !Point, rectSize :: !Size }
                deriving Show

instance Storable Rect where
    sizeOf _ = #size CGRect
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        rectOrigin <- (#peek CGRect, origin) p
        rectSize <- (#peek CGRect, size) p
        return Rect {..}
    poke p Rect {..} = do
        (#poke CGRect, origin) p rectOrigin
        (#poke CGRect, size) p rectSize



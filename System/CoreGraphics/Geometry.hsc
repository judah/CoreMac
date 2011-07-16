module System.CoreGraphics.Geometry where

#include <ApplicationServices/ApplicationServices.h>

type CGFloat = #type CGFloat

data Point = Point { pointX, pointY :: !CGFloat }
data Size = Size { sizeWidth , sizeHeight :: !CGFloat }
data Rect = Rect { rectOrigin :: !Point, rectSize :: !Size }

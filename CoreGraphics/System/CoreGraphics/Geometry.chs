module System.CoreGraphics.Geometry where

import Foreign.Storable
import Foreign.C

#include <ApplicationServices/ApplicationServices.h>

type CGFloat = {#type CGFloat #}

data Point = Point { pointX, pointY :: !CGFloat }
                deriving Show

instance Storable Point where
    sizeOf _ = {#sizeof CGPoint #}
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        pointX <- {#get CGPoint.x #} p
        pointY <- {#get CGPoint.y #} p
        return Point {..}
    poke p Point {..} = do
        {#set CGPoint.x#} p pointX
        {#set CGPoint.y#} p pointY

data Size = Size { sizeWidth , sizeHeight :: !CGFloat }
                deriving Show

instance Storable Size where
    sizeOf _ = {#sizeof CGSize #}
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        sizeWidth <- {#get CGSize.width#} p
        sizeHeight <- {#get CGSize.height#} p
        return Size {..}
    poke p Size {..} = do
        {#set CGSize.width#} p sizeWidth
        {#set CGSize.height#} p sizeHeight


data Rect = Rect { rectOrigin :: !Point, rectSize :: !Size }
                deriving Show

instance Storable Rect where
    sizeOf _ = {#sizeof CGRect #}
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        pointX <- {#get CGRect.origin.x #} p
        pointY <- {#get CGRect.origin.y #} p
        sizeWidth <- {#get CGRect.size.width #} p
        sizeHeight <- {#get CGRect.size.height #} p
        return Rect {rectOrigin=Point{..},rectSize=Size{..}}
    poke p Rect {..} = do
        {#set CGRect.origin.x #} p (pointX rectOrigin)
        {#set CGRect.origin.y #} p (pointY rectOrigin)
        {#set CGRect.size.width #} p (sizeWidth rectSize)
        {#set CGRect.size.height #} p (sizeHeight rectSize)



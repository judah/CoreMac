module System.CoreGraphics.AffineTransform where

import System.CoreGraphics.Geometry
import Foreign
import Foreign.C

#include <ApplicationServices/ApplicationServices.h>

-- | An affine transformation.  
-- 
-- >   x' = a x + c y + tx
-- >   y' = b x + d y + ty
-- 
-- Alternately:
-- 
-- >   T = (a  b  0)
-- >       (c  d  0)
-- >       (tx ty 1)
-- >
-- > (x' y' 1) = (x y 1) T
data AffineTransform = AffineTransform {
                                matrixA, matrixB, matrixC, matrixD,
                                matrixTX, matrixTY :: !CGFloat
                                }

instance Storable AffineTransform where
    sizeOf _ = {#sizeof CGAffineTransform #}
    alignment _ = alignment (undefined :: CGFloat)
    peek p = do
        matrixA <- {#get CGAffineTransform->a#} p
        matrixB <- {#get CGAffineTransform->b#} p
        matrixC <- {#get CGAffineTransform->c#} p
        matrixD <- {#get CGAffineTransform->d#} p
        matrixTX <- {#get CGAffineTransform->tx#} p
        matrixTY <- {#get CGAffineTransform->ty#} p
        return AffineTransform {..}
    poke p AffineTransform {..} = do
        {#set CGAffineTransform->a#} p matrixA
        {#set CGAffineTransform->b#} p matrixB
        {#set CGAffineTransform->c#} p matrixC
        {#set CGAffineTransform->d#} p matrixD
        {#set CGAffineTransform->tx#} p matrixTX
        {#set CGAffineTransform->ty#} p matrixTY

-- | For testing:
applyTransform :: AffineTransform -> Point -> Point
applyTransform AffineTransform {..} Point {..} = Point {
    pointX = matrixA * pointX + matrixC * pointY + matrixTX,
    pointY = matrixB * pointX + matrixD * pointY + matrixTY
    }


identity :: AffineTransform
identity = AffineTransform 1 0 0 1 0 0

translate :: CGFloat -> CGFloat -> AffineTransform
translate tx ty = AffineTransform 1 0 0 1 tx ty

scale :: CGFloat -> CGFloat -> AffineTransform
scale sx sy = AffineTransform sx 0 0 sy 0 0

-- | Rotate counterclockwise by an angle in radians.
-- 
-- Note that on OS X, this is the opposite of the standard CoreGraphics functions
-- which rotate clockwise.  (On iOS, the standard functions rotate counter-clockwise.)
rotate :: CGFloat -> AffineTransform
rotate angle = AffineTransform c s (negate s) c 0 0
  where
    c = cos angle
    s = sin angle



-- | @(f \`compose\` g) \`applyTransform\` p
-- = f \`applyTranform\` (g \`applyTransform\` p)@
compose :: AffineTransform -> AffineTransform -> AffineTransform
compose t t' = AffineTransform {
                -- Transform evaluation means multiplying the matrix by a
                -- point on the left.
                -- So (t `compose` t')`applyTransform` x 
                --    == x t' t = x (t' * t) (where "*" is the standard matrix product)
                matrixA = matrixA t' * matrixA t + matrixB t' * matrixC t,
                matrixB = matrixA t' * matrixB t + matrixB t' * matrixD t,
                matrixC = matrixC t' * matrixA t + matrixD t' * matrixC t,
                matrixD = matrixC t' * matrixB t + matrixD t' * matrixD t,
                matrixTX = matrixTX t' * matrixA t + matrixTY t' * matrixC t
                                    + matrixTX t,
                matrixTY = matrixTX t' * matrixB t + matrixTY t' * matrixD t
                                    + matrixTY t
            }

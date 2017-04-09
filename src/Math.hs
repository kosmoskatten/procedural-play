module Math where

import           Linear (V2 (..), V3 (..))

baryCentricHeight :: (Fractional a, Num a) => V3 a -> V3 a -> V3 a -> V2 a -> a
baryCentricHeight (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) (V2 x y) =
    let det = (z2 - z3) * (x1 - x3) + (x3 - x2) * (z1 - z3)
        l1 = ((z2 - z3) * (x - x3) + (x3 - x2) * (y - z3)) / det
        l2 = ((z3 - z1) * (x - x3) + (x1 - x3) * (y - z3)) / det
        l3 = 1.0 - l1 - l2
    in l1 * y1 + l2 * y2 + l3 * y3

baryCentricHeight2 :: (Fractional a, Num a) => V3 a -> V3 a -> V3 a -> V2 a -> a
baryCentricHeight2 = undefined

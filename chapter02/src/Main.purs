module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)
import Math (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

-- 2.1 (Easy) Use the Math.pi constant to write a function circleArea which
-- computes the area of a circle with a given radius. Test your function using
-- PSCi (Hint: don’t forget to import Math.pi by modifying the import Math
-- statement).
circleArea :: Number -> Number
circleArea r = pi * r * r

main :: forall a. Eff (console :: CONSOLE | a) Unit
main = print (circleArea 12.0)

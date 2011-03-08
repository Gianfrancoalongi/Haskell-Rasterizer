
{-      ************************************************
        Prefabricated (prefab) module.
        Contains objects and such.
        Gianfranco Alongi AKA zenon             20071112
        gianfranco.alongi@gmail.com
        ************************************************
-}

module Prefabs where

import Graphics.HGL
import Engine
import List 

-- -----------------------------------------------------------------------

-- An isometric cube (quadratic) vid size s and color c, given point
-- is top left backside corner of cube
cube :: RPoint -> Scale -> Color -> [RotationPoint] -> Object
cube (x,y,z) s color rots = Object 
       [ [(x,y,z),(x+s,y,z),(x+s,y+s,z),(x,y+s,z),(x,y,z)],       --Back
         [(x,y,z+s),(x+s,y,z+s),(x+s,y+s,z+s),(x,y+s,z+s),(x,y,z+s)], --Front
         [(x,y,z),(x+s,y,z),(x+s,y,z+s),(x,y,z+s),(x,y,z)],           --Top
         [(x,y+s,z),(x+s,y+s,z),(x+s,y+s,z+s),(x,y+s,z+s),(x,y+s,z)]  --Bottom
       ] color rots

-- A two dimensional function, plotted from x y z
-- Function result is on Y axis, x input is X, y input is Z
function :: RPoint -> (Double -> Double -> Double) -> 
            [Double] -> [Double] -> Scale -> StepSize -> Color -> Object
function (x,y,z) f xs ys s n c = Object (gra++(transpose gra))
                         c 
                        [(RotPoint (x+(l/2),y+(l/2),z+(l/2)) Y 20)]
        where
        xs' = takeNth xs n              -- "Stepsize n"
        ys' = takeNth ys n              -- "Stepsize n"
        l   = ((last xs')-(head xs'))*s      -- Rotation point factor
        gra = [[(x+x'*s,y+(f x' y')*s,z+y'*s) | x' <- xs'] |
              y' <- ys']        -- Generate points

takeNth :: [a] -> Int -> [a]
takeNth l n = [ l' | (i,l') <- zip [1..] l,i `mod` n == 0]

lengthD :: [a] -> Double
lengthD [] = 0.0
lengthD (x:xs) = 1.0 + lengthD xs

-- Drifr caused if rotations on different points and different axis
-- are combined

{- 
  BAD. Drifting
 [(RotPoint (0,0,0) X ang),
                 (RotPoint (x+(s/2),y+(s/2),z+(s/2)) Z  ang)]
-}

{- OK. stays in place
                [(RotPoint (0,0,0) X  ang),
                (RotPoint (0,0,0) Y  ang),
                (RotPoint (0,0,0) Z ang)]
-}

{- OK. stays in place
                [(RotPoint (x+(s/2),y+(s/2),z+(s/2)) X  ang),
                (RotPoint (x+(s/2),y+(s/2),z+(s/2)) Y  ang),
                (RotPoint (x+(s/2),y+(s/2),z+(s/2)) Z  ang)]
-}


-- A pyramid with sides l and color c, given point is top point of pyramid
pyramid :: RPoint -> Scale -> Color -> Object
pyramid (x,y,z) s color = Object
        [ [(x,y,z),(x,y+s,z+s),(x+s,y+s,z-s),(x,y,z)],
          [(x,y,z),(x,y+s,z+s),(x-s,y+s,z-s),(x,y,z)],
          [(x,y,z),(x+s,y+s,z-s),(x-s,y+s,z-s),(x,y,z)]
        ] color []



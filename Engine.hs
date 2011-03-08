
{-      
        ******************************************
        3D Math module.
        Gianfranco Alongi AKA zenon       20071112
        gianfranco.alongi@gmail.com     
        ******************************************
-}


module Engine where

import Graphics.HGL
import List             -- Transpose function, neat

-- -----------------------------------------------------------------------
-- Types and such

-- An Axis.
data Axis      = X | Y | Z
        deriving(Show,Eq)

-- A rotation specification. A point in the space with the axis 
-- specified and the angle
data RotationPoint = RotPoint RPoint Axis  Angle
        deriving(Show,Eq)

type StepSize   = Int
type RPoint     = (Double,Double,Double)     -- X,Y,Z
type Scale      = Double                     -- A scale (size)
type Part       = [RPoint]                   -- A part of an object
type Matrix     = [[Double]]                 -- A matrix


-- A 3D object, consists of Parts and a color, and a list of rotation
-- axises (how to rotate the object)
data Object  = Object {parts::[Part],
                       color::Color,
                     rotates::[RotationPoint]}
        deriving(Show)


-- A camera has a position in the Environ, a rotation and also
-- the observer point
data Camera  = Camera {point::RPoint, rotation::RPoint, viewer::RPoint}
        deriving(Show)

-- The Environment, has objects and a camera
data Engine = Engine {objects::[Object],camera::Camera}
        deriving(Show)

-- -----------------------------------------------------------------------
-- Backbone of the engine

-- Draws all object onto the window. Translates all objects into
-- graphics, overlays them and draws them
drawWorld :: Window -> Engine -> IO ()
drawWorld w e = drawInWindow w $ 
                overGraphics [ obj2Graphic w e o | 
                               o <- objects e]


-- Create a Graphic from an entire Object
-- Draws all parts as lines
obj2Graphic :: Window -> Engine -> Object -> Graphic
obj2Graphic w e o = withColor (color o) $
                   overGraphics $
                   [ polyline $ map (project (camera e)) part
                    | part <- parts o]


-- Rotate all objects in engine, if the object will rotate, it
-- does so
rotObjects :: Engine -> Engine
rotObjects (Engine o c) = Engine (map rotObjPoints o) c

-- Rotates all points of object 
rotObjPoints :: Object -> Object
rotObjPoints (Object ps c rs) = Object ps' c rs
        where
        ps' = map (\prt -> map (\pnt -> rotObjPoint rs pnt) prt )ps

-- Rotates one object point, by all rotations
rotObjPoint :: [RotationPoint] -> RPoint -> RPoint
rotObjPoint rotations p  = (x',y',z')
        where
        [[x'],[y'],[z'],[1]] = rot4x4 rotations $ rp4x4 p
                
-- Create 4x4 matrices from several rotation point specifications,
-- combine into one final result. If no rotation is made,
-- just return a quaternion representing the point (x,y,z)
-- Calls oneQuatMul repeatedly
rot4x4 :: [RotationPoint] -> Matrix -> Matrix
rot4x4 []       m = m
rot4x4 (rp:rps) m = rot4x4 rps m'
        where
        m' = rot4x4Mul rp m         

-- Does the [R] = T^-1 * [R0] * [T] * [P] multiplication,
-- with 4x4 matrices. Arg2 = The point as a 4x4 matrix
rot4x4Mul :: RotationPoint -> Matrix -> Matrix
rot4x4Mul (RotPoint (x,y,z) ax an) mpoint = rotated
        where
        aa      = [[1,0,0,x],           -- Translation matrix
                   [0,1,0,y],
                   [0,0,1,z],
                   [0,0,0,1]]
        bb      = [[1,0,0,-x],          -- Translation matrix
                   [0,1,0,-y],
                   [0,0,1,-z],
                   [0,0,0,1]]
        rotOrig = rotMatrix an ax       -- Local rotation matrix
        rotOrig'= [r++[0] | r <- rotOrig]++[[0,0,0,1]]
        matrix  = mm aa $ mm rotOrig' bb-- Rotation/Transl representation
        rotated = mm matrix mpoint      -- Rotated point

-- General translation, translates a 3D point by use of 4x4 matrix
-- Orthogonalize matrix
translate :: RPoint -> Matrix -> RPoint
translate p m = (x,y,z)
        where
        [[x],[y],[z],[1]] = mm m $ rp4x4 p

-- General rotater, rotates a 3D point with 3x3 rotation matrix
-- Orthogonalize matrix
rotate :: RPoint -> Matrix -> RPoint
rotate p m  = (x',y',z')
        where
        [[x'],[y'],[z']] = gramSchmidt $ mm m $ rp3x3 p

-- Creates 4x4 translation matrix from a size and an axis
transMatrix :: Scale -> Axis -> Matrix
transMatrix s a = case a of
                X       -> [[1,0,0,s],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
                Y       -> [[1,0,0,0],[0,1,0,s],[0,0,1,0],[0,0,0,1]]
                Z       -> [[1,0,0,0],[0,1,0,0],[0,0,1,s],[0,0,0,1]]

-- Creates 3x3 Axis rotation matrices
rotMatrix :: Angle -> Axis -> Matrix
rotMatrix a' ax = x 
        where
        x = case ax of
                Z       -> [[cos a,-(sin a),0],
                            [sin a,cos a,0],
                            [0,0,1]]
                X       -> [[1,0,0],
                            [0,cos a,-(sin a)],
                            [0,sin a,cos a]]
                Y       -> [[cos a, 0, sin a],
                            [0,1,0],
                            [-(sin a),0,cos a]]
                where
                a       = a'/400        -- Scale (for higher resolution)

-- Projects a 3D point into a planar point, with relation
-- to the camera and viewpoint
project :: Camera -> RPoint -> Point
project (Camera (cx,cy,cz) (rx,ry,rz) (ex,ey,ez)) (x,y,z) = (bx,by)
        where   
        a' = rotMatrix (-rx) X  --vector rotation matrix
        b' = rotMatrix (-ry) Y  --vector rotation matrix
        c' = rotMatrix (-rz) Z  --vector rotation matrix
        d' = [[x],[y],[z]]              --subtractee
        e' = [[cx],[cy],[cz]]           --subtractor
        [[dx],[dy],[dz]] = mm a' $ mm b' $ mm c' $ ms d' e'
        bx = round $ (dx-ex)*(ez/dz)    --to plane
        by = round $ (dy-ey)*(ez/dz)    --to plane


-- Creates a 3x3 matrix [[x],[y],[z]] from an RPoint
rp3x3 :: RPoint -> Matrix
rp3x3 (x,y,z) = [[x],[y],[z]]

-- Creates a 4x4 matrix [[x],[y],[z],[1]] from a 3D Point (x,y,z)
rp4x4 :: RPoint -> Matrix
rp4x4 (x,y,z) = [[x],[y],[z],[1]]

-- Matrix multiplication 
mm :: Matrix -> Matrix -> Matrix
mm a b  = [[sum$zipWith (*) row col | col <- transpose b] | row <-a]

-- Matrix subtraction
ms :: Matrix -> Matrix -> Matrix
ms a b  = [ concat[ zipWith (-) row row' | row' <- take 1$drop ri b]
             | (row,ri) <-zip a [0..]]

-- Gram Schmidt algorithm for orthogonalizing matrices
gramSchmidt :: Matrix -> Matrix
gramSchmidt m = [normVect $ updt v_j (take (j-1) m)
                | (j,v_j) <- zip [0..] m ]
     where
     updt :: [Double] -> Matrix -> [Double]
     updt v_j'  []           = v_j'
     updt v_j' (v_i:vs)      = updt v vs
          where
          v = zipWith (-) v_j' (zipWith (*) (zipWith (*) v_i v_j') v_i)


-- Normalize a vector
normVect :: [Double] -> [Double]
normVect []     = []
normVect a      = [ a'/l | a' <- a]
                where
                l = sqrt $ sum [a'*a'|a'<-a]
-- BUG::
-- Objects "drift". Caused by several rotations
--

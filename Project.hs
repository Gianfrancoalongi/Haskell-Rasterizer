
{--     ********************************************
        3D Rotating - Translating demo
        Gianfranco Alongi AKA zenon
        gianfranco.alongi@gmail.com         20071111
        ********************************************
-}

import Graphics.HGL
import Debug.Trace
import Engine           -- Mathematical backbone
import Prefabs          -- Prefrabricated types

-- -----------------------------------------------------------------------

xP      = -60
yP      = -20
scale   = 10
xv      = map (\x -> x/10) [0..120]
yv      = map (\x -> x/10) [0..120]
ang     = 30
a       = [(RotPoint (0,0,0) X ang),
           (RotPoint (10,10,10) Y ang),
           (RotPoint (0,0,0) Z ang)]
b       = [(RotPoint (10,10,10) X ang),
           (RotPoint (0,0,0) Y ang),
           (RotPoint (0,0,0) Z ang)]
c       = [(RotPoint (0,0,0) X ang),
           (RotPoint (0,0,0) Y ang),
           (RotPoint (10,10,10) Z ang)]


f x y = (sin y + cos x)
e = Engine      [
--      (function (xP,yP,-100) f xv yv scale 7 White)
        cube (-20,-20,-20) 10 White a,
	cube (20,-20,-20) 10 Red b,
	cube (0,-20,-20) 10 Blue c
                ]
              (Camera (90,70,180) (0,0,0) (0,0,700))


-- -----------------------------------------------------------------------

main =runGraphics $
        do w <- openWindowEx "3D Objects" (Just (0,0)) 
                (718,549) DoubleBuffered (Just (20))
           tick w $ e
           closeWindow w  

-- -----------------------------------------------------------------------
-- World specific code

-- Tick the world
tick w e = do   getWindowTick w
                clearWindow w
                drawWorld w e
                -- Auto tick
--                getKey w
                tick w $ rotObjects e
                -- Read input and determine action
--                k <- getKey w
--                trace (show $ camera e) keyCap k w e

delta = 10
-- What to do if key pressed
keyCap :: Key -> Window -> Engine -> IO ()
keyCap k w e
            -- Move camera, arrows
            | isLeftKey k  = tick w $ moveCam e X (-delta)
            | isRightKey k = tick w $ moveCam e X (delta)
            | isUpKey    k = tick w $ moveCam e Y (-delta)
            | isDownKey k  = tick w $ moveCam e Y (delta)    
            | isPageUpKey k= tick w $ moveCam e Z (delta)    
            | isPageDownKey k=tick w $ moveCam e Z (-delta)    
            | isCharKey k  = case keyToChar k of
            -- Move oberserver point (wasd)
                        'a'     -> tick w $ moveEye e X (-delta)
                        'd'     -> tick w $ moveEye e X (delta)
                        'w'     -> tick w $ moveEye e Y (-delta)
                        's'     -> tick w $ moveEye e Y (delta)
                        'q'     -> tick w $ moveEye e Z (-delta)
                        'e'     -> tick w $ moveEye e Z (delta)
            -- Tilt camera (ui,jk,nm)
                        'u'     -> tick w $ tiltCam e X (-0.1)
                        'i'     -> tick w $ tiltCam e X 0.1
                        'j'     -> tick w $ tiltCam e Y (-0.1)
                        'k'     -> tick w $ tiltCam e Y 0.1
                        'n'     -> tick w $ tiltCam e Z (-0.1)
                        'm'     -> tick w $ tiltCam e Z 0.1
                        _       -> tick w e
            | otherwise    = tick w e


-- Move observer point in Camera space
moveEye :: Engine -> Axis -> Scale -> Engine
moveEye (Engine o c) d s = Engine o (moveEye' c d s)
        where
        moveEye' c d f = Camera (point c) (rotation c) view_point
                where
                view_point = translate (viewer c) $ transMatrix s d

-- Move camera in space
moveCam :: Engine -> Axis -> Scale -> Engine
moveCam (Engine o c) d s = Engine o (moveCam' c d s)
        where
        moveCam' c d f = Camera cam_point (rotation c) (viewer c)
                where
                cam_point = translate (point c) $ transMatrix s d 

-- Tilt camera in space
tiltCam :: Engine -> Axis -> Scale -> Engine
tiltCam (Engine o c) d s = Engine o (tiltCam' c d s)
        where
        tiltCam' c d f = Camera (point c) tilt_p (viewer c)
                where
                tilt_p =  translate (rotation c) $ transMatrix s d



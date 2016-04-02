{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.IORef
import Control.Monad.Loops
import qualified Data.Set as S
import Control.Monad.Reader
import Prelude hiding (init,)
import Control.Concurrent
import Data.Colour.SRGB (sRGB24read)
import Diagrams.Backend.NanoVG
import Diagrams.Prelude
import Data.Bits hiding (rotate)
import NanoVG hiding (rotate,circle,scale,width)
import Control.Monad
import Graphics.GL.Core32
import Foreign.C.Types
import Graphics.UI.GLFW

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

blue2, blue3, blue6, blue7 :: Colour Double
[ blue2, blue3, blue6, blue7] =
  map sRGB24read ["#c6dbef", "#9ecae1", "#2171b5", "#08519c"]

wheel :: [Colour Double] -> Diagram B
wheel [] = circle 1 # fc black
wheel cs = wheel' # rotateBy r
  where
    wheel' = mconcat $ zipWith fc cs (iterateN n (rotateBy a) w)
    n = length cs
    a = 1 / fromIntegral n
    w = wedge 1 xDir (a @@ turn) # lw none
    r = 1/4 - 1/(2*fromIntegral n)

planet :: Angle Double -> Diagram B
planet r = circle 0.8 # fc black
        <> wheel (take 12 . cycle $ [blue3, blue6])
         # rotate r
         # lw none

planets :: Angle Double -> Diagram B
planets r
  = atPoints (trailVertices $ square 2) (repeat (planet r))
  # rotateBy (1/8) # centerXY

sun :: Angle Double -> Diagram B
sun r = w # rotate ((r^.turn / (1 - sqrt 2)) @@ turn)
  where
    w = circle 0.3 # fc black <> wheel (take 60 . cycle
      $ [ blue2, blue7])
      # scale (sqrt 2 -1)
      # rotateBy (1/8)

solar :: Angle Double -> Diagram B
solar r = bg black . pad 1.1 . centerXY $ sun r <> planets r

main :: IO ()
main = do ref <- newIORef 0
          forkIO (animateGear ref)
          canvasRender ref

animateGear :: IORef Double -> IO ()
animateGear ref = loop 1
  where loop n = threadDelay 2000 >> atomicWriteIORef ref n >> loop (n + 1)

canvasRender :: IORef Double -> IO ()
canvasRender ref =
  do e <- init
     when (not e) $ putStrLn "Failed to init GLFW"
     windowHint $ WindowHint'ContextVersionMajor 3
     windowHint $ WindowHint'ContextVersionMinor 2
     windowHint $ WindowHint'OpenGLForwardCompat True
     windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
     windowHint $ WindowHint'OpenGLDebugContext True
     win <-
       createWindow 800
                    600
                    "NanoVG"
                    Nothing
                    Nothing
     case win of
       Nothing -> putStrLn "Failed to create window" >> terminate
       Just w ->
         do makeContextCurrent win
            glewInit
            glGetError
            cxt <- createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
            -- error handling? who needs that anyway
            swapInterval 0
            setTime 0
            whileM_ (not <$> windowShouldClose w) $
              do Just t <- getTime
                 (mx,my) <- getCursorPos w
                 (width,height) <- getWindowSize w
                 (fbWidth,fbHeight) <- getFramebufferSize w
                 n <- readIORef ref
                 let pxRatio = fromIntegral fbWidth / fromIntegral width
                 glViewport 0
                            0
                            (fromIntegral fbWidth)
                            (fromIntegral fbHeight)
                 glClearColor 0.3 0.3 0.32 1.0
                 glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|.
                          GL_STENCIL_BUFFER_BIT)
                 beginFrame cxt
                            (fromIntegral width)
                            (fromIntegral height)
                            pxRatio
                 runReaderT
                   (renderDia NanoVG
                              (NanoVGOptions
                                 (mkSizeSpec2D (Just $ fromIntegral width)
                                               (Just $ fromIntegral height)))
                              (solar (n/288 @@ turn)))
                   cxt
                 -- TODO insert code
                 endFrame cxt
                 swapBuffers w
                 pollEvents
-- loop :: DeviceContext -> Double -> IO a
-- loop context n = do
--   send context $ renderDia NanoVG (NanoVGOptions (mkWidth 500))
--                                   (solar (n/588 @@ turn))
--   threadDelay 2000
--   loop context (n + 1)

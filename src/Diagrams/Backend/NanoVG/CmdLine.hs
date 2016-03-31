{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.NanoVG.CmdLine
-- Copyright   :  (c) 2011-2014 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the NanoVG backend.
--
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a suitable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.NanoVG.CmdLine
       (
        -- * General form of @main@
        --  $mainWith
        mainWith

        -- * Supported froms of @main@
       , defaultMain
       , multiMain
       , NanoVG
       , B
       ) where

import Control.Monad.Reader
import qualified Data.Set as S
import Control.Monad
import           Diagrams.Backend.CmdLine hiding (height, width)
import           Diagrams.Backend.NanoVG
import           Diagrams.Prelude         hiding (height, option, value, width,
                                           (<>),Context)
import Foreign.C.Types
import Graphics.GL.Core32
import Graphics.UI.GLFW
import Prelude hiding (init)
import NanoVG hiding (width)
import Data.Bits
import Control.Monad.Loops

import           Data.Data
import           Options.Applicative

data DiaOpts = DiaOpts
  { _width  :: Int -- ^ Final output width of diagram.
  , _height :: Int -- ^ Final height of diagram.
  } deriving (Show, Data, Typeable)

makeLenses ''DiaOpts

diaOpts :: Parser DiaOpts
diaOpts = DiaOpts
  <$> (option auto)
      (long "width" <> short 'w'
    <> metavar "WIDTH"
    <> help "Desired WIDTH of the output image")
  <*> (option auto)
      (long "height" <> short 'h'
    <> metavar "HEIGHT"
    <> help "Desired HEIGHT of the output image")

instance Parseable DiaOpts where
  parser = diaOpts

defaultMain :: QDiagram NanoVG V2 Double Any -> IO ()
defaultMain = mainWith

instance Mainable (QDiagram NanoVG V2 Double Any) where
  type MainOpts (QDiagram NanoVG V2 Double Any) = DiaOpts

  mainRender = canvasRender

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

canvasRender :: DiaOpts -> QDiagram NanoVG V2 Double Any -> IO ()
canvasRender opts d =
  do e <- init
     when (not e) $ putStrLn "Failed to init GLFW"
     windowHint $ WindowHint'ContextVersionMajor 3
     windowHint $ WindowHint'ContextVersionMinor 2
     windowHint $ WindowHint'OpenGLForwardCompat True
     windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
     windowHint $ WindowHint'OpenGLDebugContext True
     win <-
       createWindow (opts ^. width)
                    (opts ^. height)
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
                              d)
                   cxt
                 -- TODO insert code
                 endFrame cxt
                 swapBuffers w
                 pollEvents

multiMain :: [(String, QDiagram NanoVG V2 Double Any)] -> IO ()
multiMain = mainWith

instance Mainable [(String, QDiagram NanoVG V2 Double Any)] where
  type MainOpts [(String, QDiagram NanoVG V2 Double Any)] =
    (MainOpts (QDiagram NanoVG V2 Double Any), DiagramMultiOpts)

  mainRender = defaultMultiMainRender

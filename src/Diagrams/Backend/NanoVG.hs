{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-} 
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.NanoVG
-- Copyright   :  (c) 2010 - 2014 diagrams-canvas team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using Canvas.
-- Implemented using the blank-canvas platform.
--
-- To invoke the Canvas backend, you have three options.
--
-- * You can use the "Diagrams.Backend.Canvas.CmdLine" module to create
--   standalone executables which will display the diagram in a browser
--   using a web service.
--
-- * You can use the 'renderCanvas' function provided by this module,
--   which gives you more programmatic control over when and
--   how images are displayed (making it east to, for example, write a
--   single program that displays multiple images, or one that diaplays
--   images dynamically based on user input, and so on).
--
-- * For the most flexiblity you can invoke the 'renderDia' method from
--   'Diagrams.Core.Types.Backend' instance for @Canvas@. In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v -> QDiagram b v m -> Result b v
--
-- (omitting a few type class contraints). @b@ represents the
-- backend type, @v@ the vector space, and @m@ the type of monoidal
-- query annotations on the diagram. 'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend. For @b ~ Canvas@ and @v ~ R2@, we have
--
-- > data Options Canvas V2 Double = CanvasOptions
-- >  { _size :: SizeSpec V2 -- ^^ The requested size
-- >  }
--
-- @
-- data family Render Canvas V2 Double = C (RenderM ())
-- @
--
-- @
-- type family Result Canvas V2 Double = Canvas ()
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Canvas -> Options Canvas V2 Double -> QDiagram Canvas V2 Double m ->
-- Canvas()
-- @
--
-- which you could call like @renderDia Canvas (CanvasOptions (width 250))
-- myDiagram@
--
------------------------------------------------------------------------------

module Diagrams.Backend.NanoVG

  ( NanoVG(..) -- rendering token
  , B
  , Options(..) -- for rendering options specific to Canvas

  -- , renderNanoVG

  ) where

import           Control.Lens                 hiding (transform, (#))
import           Control.Monad.Reader
import           Control.Monad.State          (when, MonadState, State, evalState, mapStateT)
import qualified Control.Monad.StateStack     as SS
import           Control.Monad.Trans          (lift)

import           Data.Default.Class
import qualified Data.Foldable                as F
import           Data.List
import           Data.Maybe                   (catMaybes, isJust, fromJust, fromMaybe)
import           Data.NumInstances            ()
import           Data.Ord
import qualified Data.Set as S
import qualified Data.Text                    as T
import           Data.Tree                    (Tree(Node))
import           Data.Typeable                (Typeable)
import           Data.Word                    (Word8)

import           Diagrams.Attributes
import           Diagrams.Prelude             hiding (fillTexture, moveTo, stroke, size, local)
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Attributes     (splitTextureFills)
import           Diagrams.TwoD.Path           (Clip (Clip))
import           Diagrams.TwoD.Text

import           Diagrams.Core.Compile
import           Diagrams.Core.Transform      (matrixHomRep)
import           Diagrams.Core.Types          (Annotation (..))

import qualified NanoVG as BC

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data NanoVG = NanoVG
    deriving (Eq, Ord, Read, Show, Typeable)

type B = NanoVG

type instance V NanoVG = V2
type instance N NanoVG = Double

data NanoVGState = NanoVGState { _accumStyle :: Style V2 Double
                               , _csPos :: (Double, Double) }

makeLenses ''NanoVGState

instance Default NanoVGState where
  def = NanoVGState { _accumStyle = mempty
                    , _csPos = (0,0) }

newtype RenderM a =
  RenderM (SS.StateStackT NanoVGState (ReaderT BC.Context IO) a)
  deriving (Functor,Applicative,Monad,MonadIO,MonadState NanoVGState,SS.MonadStateStack NanoVGState)

instance MonadReader BC.Context RenderM where
  ask  = RenderM $ lift ask
  local f (RenderM m) = RenderM $ mapStateStackT (local f) m

mapStateStackT :: (m (a,(s,[s])) -> n (b,(s,[s]))) -> SS.StateStackT s m a -> SS.StateStackT s n b
mapStateStackT f = SS.StateStackT . mapStateT f . SS.unStateStackT

runRenderM :: RenderM a -> ReaderT BC.Context IO a
runRenderM (RenderM m) = SS.evalStateStackT m def

instance Monoid (Render NanoVG V2 Double) where
  mempty  = C $ return ()
  (C c1) `mappend` (C c2) = C (c1 >> c2)

instance Backend NanoVG V2 Double where
  data Render  NanoVG V2 Double = C (RenderM ())
  type Result  NanoVG V2 Double = ReaderT BC.Context IO ()
  data Options NanoVG V2 Double = NanoVGOptions
          { _canvasSize   :: SizeSpec V2 Double   -- ^ the requested size
          }

  renderRTree :: NanoVG -> Options NanoVG V2 Double -> RTree NanoVG V2 Double Annotation
                        -> Result NanoVG V2 Double
  renderRTree _ _ rt = evalState canvasOutput initialNanoVGRenderState
    where
      canvasOutput :: State NanoVGRenderState (ReaderT BC.Context IO ())
      canvasOutput = do
        let C r = toRender rt
        return $ runRenderM $ r

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY)

runC :: Render NanoVG V2 Double -> RenderM ()
runC (C r) = r

toRender :: RTree NanoVG V2 Double Annotation -> Render NanoVG V2 Double
toRender = fromRTree
  . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  . (:[])
  . splitTextureFills
    where
      fromRTree (Node (RPrim p) _) = render NanoVG p
      fromRTree (Node (RStyle sty) rs) = C $ do
        save
        canvasStyle sty
        accumStyle %= (<> sty)
        runC $ F.foldMap fromRTree rs
        restore
      fromRTree (Node _ rs) = F.foldMap fromRTree rs

data NanoVGRenderState = NanoVGRenderState

initialNanoVGRenderState :: NanoVGRenderState
initialNanoVGRenderState = NanoVGRenderState

getSize :: Options NanoVG V2 Double -> SizeSpec V2 Double
getSize (NanoVGOptions {_canvasSize = s}) = s

setSize :: Options NanoVG V2 Double -> (SizeSpec V2 Double) -> Options NanoVG V2 Double
setSize o s = o {_canvasSize = s}

size :: Lens' (Options NanoVG V2 Double)(SizeSpec V2 Double)
size = lens getSize setSize

move :: (Double, Double) -> RenderM ()
move p = do csPos .= p

save :: RenderM ()
save = SS.save >> (liftIO . BC.save =<< ask)

restore :: RenderM ()
restore = (liftIO . BC.restore =<< ask) >> SS.restore

newPath :: RenderM ()
newPath = liftIO . BC.beginPath =<< ask

closePath :: RenderM ()
closePath = liftIO . BC.closePath =<< ask

moveTo :: Double -> Double -> RenderM ()
moveTo x y = do
  cxt <- ask
  liftIO $ BC.moveTo cxt (realToFrac x) (realToFrac y)
  move (x, y)

relLineTo :: Double -> Double -> RenderM ()
relLineTo x y = do
  p <- use csPos
  let p'@(x',y') = p + (x, y)
  cxt <- ask
  liftIO $ BC.lineTo cxt (realToFrac x') (realToFrac y')
  move p'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> RenderM ()
relCurveTo ax ay bx by cx cy = do
  p <- use csPos
  let [(ax',ay'),(bx',by'),(cx',cy')] = map (over both realToFrac . (p +)) [(ax,ay),(bx,by),(cx,cy)]
  cxt <- ask
  liftIO $ BC.bezierTo cxt ax' ay' bx' by' cx' cy'
  move (realToFrac cx', realToFrac cy')

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

-- | From the HTML5 canvas specification regarding line width:
--
--     "On setting, zero, negative, infinite, and NaN values must be
--     ignored, leaving the value unchanged; other values must change
--     the current value to the new value.
--
--   Hence we must implement a line width of zero by simply not
--   sending a stroke command.
stroke :: RenderM ()
stroke = do
  -- The default value of 0.5 is somewhat arbitary since lineWidth should never
  -- be 'Nothing'. 0.5 is choose since it is the lower bound of the
  -- default.
  w <- fromMaybe 0.5 <$> getStyleAttrib getLineWidth
  cxt <- ask
  when (w > (0 :: Double)) (liftIO $ BC.stroke cxt)

fill :: RenderM ()
fill = liftIO . BC.fill =<< ask

clip :: RenderM ()
clip = error "clipping is not supported"

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

data ColorOrPaint = Color BC.Color | Paint BC.Paint

texture :: Texture Double -> Double -> RenderM ColorOrPaint
texture (SC (SomeColor c)) o = pure $ Color (showColorJS c o)

texture (LG g) o =
  do cxt <- ask
     case sortBy (comparing (view stopFraction))
                 (g ^. lGradStops) of
       [GradientStop c0 0,GradientStop c1 1] ->
         liftIO $
         Paint <$>
         BC.linearGradient cxt
                           x0
                           y0
                           x1
                           y1
                           (showColorJS c0 o)
                           (showColorJS c1 o)
       _ -> error "Only two color stops are supported" -- TODO throw an exception
  where
        (x0,y0) =
          over both realToFrac $
          unp2 $
          transform (g ^. lGradTrans)
                    (g ^. lGradStart)
        (x1,y1) =
          over both realToFrac $
          unp2 $
          transform (g ^. lGradTrans)
                    (g ^. lGradEnd)
        stops =
          map (\s ->
                 (s ^. stopFraction
                 ,showColorJS (s ^. stopColor)
                              1))
              (g ^. lGradStops)

texture (RG g) o =
  do cxt <- ask
     when ((x0,y0) /= (x1,y1)) $
       error "Inner and outer circle center have two be the same"
     case sortBy (comparing (view stopFraction))
                 (g ^. rGradStops) of
       [GradientStop c0 0,GradientStop c1 1] ->
         liftIO $
         Paint <$>
         BC.radialGradient cxt
                           x0
                           y0
                           r0
                           r1
                           (showColorJS c0 o)
                           (showColorJS c1 o)
       _ -> error "Only two color stops are supported" -- TODO throw an exception
  where (r0,r1) =
          over both realToFrac $ (s * g ^. rGradRadius0,s * g ^. rGradRadius1)
        (x0,y0) =
          over both realToFrac $
          unp2 $
          transform (g ^. rGradTrans)
                    (g ^. rGradCenter0)
        (x1,y1) =
          over both realToFrac $
          unp2 $
          transform (g ^. rGradTrans)
                    (g ^. rGradCenter1)
        s = avgScale $ g ^. rGradTrans

showColorJS :: (Color c) => c -> Double  -> BC.Color
showColorJS c o = 
    BC.rgbaf
    (realToFrac r)
    (realToFrac g)
    (realToFrac b)
    (realToFrac (a*o))
  where (r,g,b,a) = colorToSRGBA . toAlphaColour $  c

canvasTransform :: T2 Double -> RenderM ()
canvasTransform tr = ask >>= \cxt -> 
                         liftIO $ BC.transform cxt
                                               (realToFrac ax) (realToFrac ay)
                                               (realToFrac bx) (realToFrac by)
                                               (realToFrac tx) (realToFrac ty)
    where
      [[ax, ay], [bx, by], [tx, ty]] = matrixHomRep tr

strokeTexture :: Texture Double -> Double -> RenderM ()
strokeTexture txt o = do
  cxt <- ask
  colorOrPaint <- texture txt o
  case colorOrPaint of
    Color c -> liftIO $ BC.strokeColor cxt c
    Paint p -> liftIO $ BC.strokePaint cxt p

fillTexture :: Texture Double -> Double -> RenderM ()
fillTexture txt o = do
  cxt <- ask
  colorOrPaint <- texture txt o
  case colorOrPaint of
    Color c -> liftIO $ BC.fillColor cxt c
    Paint p -> liftIO $ BC.fillPaint cxt p

fromLineCap :: LineCap -> BC.LineCap
fromLineCap LineCapRound  = BC.Round
fromLineCap LineCapSquare = BC.Square
fromLineCap _             = BC.Butt

-- TODO: separate linecap and linejoin?
fromLineJoin :: LineJoin -> BC.LineCap
fromLineJoin LineJoinRound = BC.Round
fromLineJoin LineJoinBevel = BC.Bevel
fromLineJoin _             = BC.Miter

showFontJS :: FontWeight -> FontSlant -> Double -> String -> T.Text
showFontJS wgt slant sz fnt = T.concat [a, " ", b, " ", c, " ", d]
  where
    a = case wgt of
          FontWeightNormal -> ""
          FontWeightBold   -> "bold"
    b = case slant of
          FontSlantNormal  -> ""
          FontSlantItalic  -> "italic"
          FontSlantOblique -> "oblique"
    c = T.concat [T.pack $ show sz, "pt"]
    d = T.pack fnt

renderC :: (Renderable a NanoVG, V a ~ V2, N a ~ Double) => a -> RenderM ()
renderC a = case (render NanoVG a) of C r -> r

canvasStyle :: Style v Double  -> RenderM ()
canvasStyle s = sequence_
              . catMaybes $ [ handle clip'
                            -- , handle lWidth
                            , handle lCap
                            , handle lJoin
                            ]
  where handle :: (AttributeClass a) => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        clip'    = mapM_ (\p -> canvasPath p >> clip) . op Clip
        -- TODO not sure what to do about this
        -- lWidth   = liftIO . BC.lineWidth . getLineWidth
        lCap cap     = ask >>= \cxt -> liftIO . BC.lineCap cxt . fromLineCap . getLineCap $ cap
        lJoin join   = ask >>= \cxt -> liftIO . BC.lineJoin cxt . fromLineJoin . getLineJoin $ join

instance Renderable (Segment Closed V2 Double) NanoVG where
  render _ (Linear (OffsetClosed (V2 x y))) = C $ relLineTo x y
  render _ (Cubic (V2 x1 y1)
                  (V2 x2 y2)
                  (OffsetClosed (V2 x3 y3)))
    = C $ relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) NanoVG where
  render _ = withTrail renderLine renderLoop
    where
      renderLine ln = C $ do
        mapM_ renderC (lineSegments ln)
      renderLoop lp = C $ do
        case loopSegments lp of
          (segs, Linear _) -> mapM_ renderC segs
          _ -> mapM_ renderC (lineSegments . cutLoop $ lp)
        closePath

instance Renderable (Path V2 Double) NanoVG where
  render _ p = C $ do
    canvasPath p
    f <- getStyleAttrib getFillTexture
    s <- getStyleAttrib getLineTexture
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    save
    when (isJust f) (fillTexture (fromJust f) (realToFrac o) >> fill)
    strokeTexture (fromMaybe (SC (SomeColor (black :: Colour Double))) s) (realToFrac o)
    stroke
    restore

-- Add a path to the NanoVG context, without stroking or filling it.
canvasPath :: Path V2 Double -> RenderM ()
canvasPath (Path trs) = do
    newPath
    F.mapM_ renderTrail trs
  where
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      uncurry moveTo p
      renderC tr

instance Renderable (Text Double) NanoVG where
  render _ (Text tr al str) = C $ do
    cxt     <- ask
    tf      <- fromMaybe "Calibri" <$> getStyleAttrib getFont
    sz      <- fromMaybe 12 <$> getStyleAttrib getFontSize
    slant   <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
    fw      <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
    tx      <- fromMaybe (SC (SomeColor (black :: Colour Double)))
               <$> getStyleAttrib getFillTexture
    o       <- fromMaybe 1 <$> getStyleAttrib getOpacity
    let fSize = avgScale tr * sz
        fnt = showFontJS fw slant fSize tf
        vAlign = case al of
                   BaselineText -> S.singleton BC.AlignBaseline
                   BoxAlignedText _ h -> case h of
                     h' | h' <= 0.25 -> S.fromList [BC.AlignBottom,BC.AlignBaseline]
                     h' | h' >= 0.75 -> S.fromList [BC.AlignTop,BC.AlignBaseline]
                     _ -> S.fromList [BC.AlignMiddle,BC.AlignBaseline]
        hAlign = case al of
                   BaselineText -> S.singleton BC.AlignLeft
                   BoxAlignedText w _ -> case w of
                     w' | w' <= 0.25 -> S.singleton BC.AlignLeft
                     w' | w' >= 0.75 -> S.singleton BC.AlignRight
                     _ -> S.singleton BC.AlignCenter
    save
    liftIO $ BC.textAlign cxt  (hAlign <> vAlign)
    liftIO $ BC.fontFace cxt fnt
    fillTexture tx (realToFrac o)
    canvasTransform (tr <> reflectionY)
    liftIO $ BC.text cxt 0 0 (T.pack str)
    restore

instance Renderable (DImage Double External) NanoVG where
  render _ (DImage path w h tr) = C $ do
    let ImageRef file = path
    save
    canvasTransform (tr <> reflectionY)
    cxt <- ask
    -- TODO error handling
    Just img <- liftIO $ BC.createImage cxt (BC.FileName (T.pack file)) 0
    imgPattern <- liftIO $ BC.imagePattern cxt 0 0 (fromIntegral w) (fromIntegral h) 0 img 1
    liftIO $ BC.rect cxt (fromIntegral (-w) /2 ) (fromIntegral (-h) /2) (fromIntegral w) (fromIntegral h)
    liftIO $ BC.fillPaint cxt imgPattern
    restore

-- renderNanoVG :: Int -> SizeSpec V2 Double -> QDiagram NanoVG V2 Double Any -> IO ()
-- renderNanoVG port sizeSpec d = BC.blankNanoVG (fromIntegral port) . flip BC.send $ img
--     where
--       img = renderDia NanoVG (NanoVGOptions sizeSpec) d

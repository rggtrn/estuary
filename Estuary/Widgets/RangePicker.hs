{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI #-}

module InnerEar.Widgets.RangePicker where

import Reflex
import Reflex.Dom
import qualified GHCJS.Types as T
import GHCJS.DOM.Types
import GHCJS.Marshal.Pure (pToJSVal)

foreign import javascript unsafe
  "$r = $1.width"
  getWidth :: HTMLCanvasElement -> IO Float

foreign import javascript unsafe
  "$r = $1.height"
  getHeight :: HTMLCanvasElement -> IO Float

foreign import javascript unsafe
  "$1.getContext('2d').clearRect(0,0,$2,$3)"
  clearCanvas :: HTMLCanvasElement -> Float -> Float -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').rect($2,$3,$4,$5)"
  rect :: HTMLCanvasElement -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').stroke()"
  stroke :: HTMLCanvasElement -> IO ()

foreign import javascript unsafe
  "$1.getContext('2d').fill()"
  fill :: HTMLCanvasElement -> IO ()

rangePicker :: MonadWidget t m =>
  m (Dynamic t (Float,Float))

rangePicker = do
  (canvasEl,_) <- elClass' "canvas" "someClass" $ return ()
  let canvasEl' = _el_element canvasEl -- Element
  let c = G.castToHTMLCanvasElement canvasEl' -- HTMLCanvasElement
  postBuild <- getPostBuild -- m (Event t ())
  let rangeEvent = fmap (const (0.25,0.75)) postBuild -- (Event t (Float,Float))
  performEvent_ $ ffor rangeEvent $ \(r1,r2) -> liftIO $ do
    w <- getWidth c
    h <- getHeight c
    clearCanvas c w h
    rect c 0.0 0.0 (w*r1) h
    stroke c
    fill c
    rect c (w-((1.0-r2)*w)) 0.0  ((1.0-r2)*w) h
    stroke c
    fill c

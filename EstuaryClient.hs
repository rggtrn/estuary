{-# LANGUAGE RecursiveDo, OverloadedStrings, JavaScriptFFI #-}

module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Types.Hint
import Estuary.Protocol.Foreign
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.Navigation
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Control.Monad (liftM)
import Sound.Tidal.Context (ParamPattern,Tempo(..))
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.SuperDirt
import Estuary.WebDirt.Stream
import Estuary.WebDirt.ImageDirt
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.Terminal
import Data.Map
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Estuary.Widgets.WebSocket
import Text.JSON
import Data.Time
import Text.Read
import qualified GHCJS.Types as T
import qualified GHCJS.DOM.Types as G
import GHCJS.Marshal.Pure (pToJSVal)

import Estuary.Types.Request
import Estuary.Types.Response


main :: IO ()
main = do
  now <- Data.Time.getCurrentTime
  let defaultTempo = Tempo {at=now,beat=0.0,cps=0.5,paused=False,clockLatency=0.2}
  tempo <- newMVar defaultTempo
  let iDirt = ImageDirt
  wd <- webDirt
  sd <- superDirt
  idStream <- sampleStream iDirt tempo
  wdStream <- sampleStream wd tempo
  sdStream <- sampleStream sd tempo
  protocol <- estuaryProtocol
  mainWidget $ estuaryWidget tempo wd wdStream sd sdStream iDirt idStream protocol now


drawOurLine :: G.HTMLCanvasElement -> IO ()
drawOurLine c = do
  let c' = G.unHTMLCanvasElement c
  beginPath c'
  moveTo c' 400 10
  lineTo c' 415 590
  strokeStyle c' ("#FFFFFF")
  stroke c'

foreign import javascript unsafe "$1.getContext('2d').beginPath()" beginPath :: T.JSVal -> IO ()
foreign import javascript unsafe "$1.getContext('2d').moveTo($2,$3)" moveTo :: T.JSVal -> Int -> Int -> IO ()
foreign import javascript unsafe "$1.getContext('2d').lineTo($2,$3)" lineTo :: T.JSVal -> Int -> Int -> IO ()
foreign import javascript unsafe "$1.getContext('2d').stroke()" stroke :: T.JSVal -> IO ()
foreign import javascript unsafe "$1.getContext('2d').strokeStyle = $2" strokeStyle :: T.JSVal -> T.JSString -> IO()

-- drawOnOurCanvas :: MonadWidget t m => El t -> Event t [DrawingInstruction] -> m ()



estuaryWidget :: MonadWidget t m =>
  MVar Tempo -> WebDirt -> SampleStream -> SuperDirt -> SampleStream ->
  ImageDirt -> SampleStream ->
  EstuaryProtocolObject -> UTCTime -> m ()
estuaryWidget tempo wd wdStream sd sdStream iDirt idStream protocol now = divClass "estuary" $ mdo

  {- (canvas,_ ) <- elAttr' "canvas" (fromList [("class","canvas"),("width","800px"),("height","600px")]) $ return ()
  postBuild <- getPostBuild -- m Event t ()
  let blah = fmap (liftIO . (\_ -> drawOurLine (G.castToHTMLCanvasElement $ _el_element canvas))) postBuild -- m (Event t (WidgetHost m ()))
  performEvent_ blah -- performEvent_ :: MonadWidget t m => Event t (WidgetHost m ()) -> m () -}

  (sdOn,wdOn) <- header wsStatus clientCount
  (values,deltasUp,hints) <- divClass "page" $ navigation now commands deltasDown'
  commands <- divClass "chat" $ terminalWidget deltasUp deltasDown'
  (deltasDown,wsStatus) <- alternateWebSocket protocol now deltasUp
  values' <- mapDyn (toParamPattern . StackedPatterns) values
  valuesSd <- liftM updated $ combineDyn f values' sdOn
  valuesWd <- liftM updated $ combineDyn f values' wdOn
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  clientCount <- holdDyn 0 $ fmapMaybe justServerClientCount deltasDown'
  performTempoUpdates tempo hints
  performHint wd hints
  performEvent_ $ fmap (liftIO . wdStream) valuesWd
  performEvent_ $ fmap (liftIO . sdStream) valuesSd
  performEvent_ $ fmap (liftIO . idStream) $ updated values'
  where f x True = x
        f _ False = toParamPattern EmptyTransformedPattern

performTempoUpdates :: MonadWidget t m => MVar Tempo -> Event t Hint -> m ()
performTempoUpdates t h = do
  let newTempi = fmapMaybe maybeTempoHint h
  performEvent_ $ fmap (liftIO . (\x -> swapMVar t x >> return ())) newTempi

header :: (MonadWidget t m) => Dynamic t String -> Dynamic t Int -> m (Dynamic t Bool, Dynamic t Bool)
header wsStatus clientCount = divClass "header" $ do
  tick <- getPostBuild
  hostName <- performEvent $ fmap (liftIO . (\_ -> getHostName)) tick
  port <- performEvent $ fmap (liftIO . (\_ -> getPort)) tick
  hostName' <- holdDyn "" hostName
  port' <- holdDyn "" port
  divClass "logo" $ text "estuary (a TidalCycles symbiont)"
  statusMsg <- combineDyn f wsStatus clientCount
  divClass "server" $ do
    text "server: "
    dynText hostName'
    text ":"
    dynText port'
    text ": "
    dynText statusMsg
  divClass "webDirt" $ divClass "webDirtMute" $ do
      text "SuperDirt:"
      sdInput <- checkbox False $ def
      text "WebDirt:"
      wdInput <- checkbox True $ def
      return (_checkbox_value sdInput,_checkbox_value wdInput)
  where
    f "connection open" c = "(" ++ (show c) ++ " clients)"
    f x _ = x

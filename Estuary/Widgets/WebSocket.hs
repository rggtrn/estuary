{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.WebSocket where

import Reflex
import Reflex.Dom
import Text.JSON
import Estuary.Protocol.JSON
import qualified Data.ByteString.Char8 as C
import Estuary.Protocol.Foreign
import Control.Monad.IO.Class (liftIO)


-- an estuaryWebSocket wraps the underlying Reflex WebSocket with some parsing of the EstuaryProtocol
-- for collaborative editing. While the password is dynamic, like the Reflex WebSocket the socket address
-- isn't (so each new address requires a new instance of the widget - see resettingWebSocket below)
-- currently not working, apparently because of a bug in the old version of reflex-dom we are using
-- (see alternateWebSocket below)

estuaryWebSocket :: MonadWidget t m => String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
estuaryWebSocket addr pwd toSend = mdo
  let addr' = "ws://" ++ addr
  let toSend' = fmap ((:[]) . C.pack . encode) $ attachDynWith setPassword pwd toSend
  ws <- webSocket addr' $ def & webSocketConfig_send .~ toSend'
  let ws' = traceEventWith (C.unpack) $ _webSocket_recv ws
  let wsRcvd = fmap (decode . C.unpack) $ ws'
  return $ fmapMaybe isOk wsRcvd
  where
    isOk (Ok x) = Just x
    isOk _ = Just (ProtocolError "unknown protocol error")


-- a resettingWebSocket is a wrapper of estuaryWebSocket above so that the webSocket address
-- is specified by event updates. A new address event causes the previous estuaryWebSocket to
-- be discarded and a new one to be created.

resettingWebSocket :: MonadWidget t m => Event t String -> Dynamic t String -> Event t EstuaryProtocol
  -> m (Event t EstuaryProtocol)
resettingWebSocket addr pwd toSend = do
  let resets = fmap (\a -> estuaryWebSocket a pwd toSend) addr
  ws <- widgetHold (return never) resets
  return $ switchPromptlyDyn ws

alternateWebSocket :: MonadWidget t m => EstuaryProtocolObject -> UTCTime -> Event t String -> Dynamic t String -> Event t EstuaryProtocol -> m (Event t EstuaryProtocol)
alternateWebSocket obj startTime addr pwd toSend = do
  -- hack <- button "hack"
  hack <- tickLossy (1.0::NominalDiffTime) startTime 
  let addr' = fmap ("ws://" ++) addr
  performEvent_ $ fmap (liftIO . (setUrl obj)) addr'
  let toSend' = attachDynWith setPassword pwd toSend
  let toSend'' = fmap (encode) toSend'
  performEvent_ $ fmap (liftIO . (send obj)) toSend''
  estuaryEdits <- performEvent $ fmap (liftIO . (\_ -> getEstuaryEdit obj 1)) hack
  textEdits <- performEvent $ fmap (liftIO . (\_ -> getTextEdit obj 2)) hack
  el "div" $ do
    text "estuaryEdits"
    (holdDyn "" estuaryEdits) >>= display
    text "textEdits"
    (holdDyn "" textEdits) >>= display
  return never


  -- getCurrentTime :: IO UTCTime

-- finally, a webSocketWidget includes GUI elements for setting the webSocket address and
-- password, and connects these GUI elements to a resettingWebSocket (i.e. estuaryWebSocket)

webSocketWidget :: MonadWidget t m => EstuaryProtocolObject -> UTCTime -> Event t EstuaryProtocol -> m (Event t EstuaryProtocol)
webSocketWidget obj startTime toSend = do
  addr <- textInput $ def & textInputConfig_initialValue .~ "127.0.0.1:8002"
  let addr' = tagDyn (_textInput_value addr) (_textInput_keypress addr)
  pwd <- textInput $ def & textInputConfig_initialValue .~ "blah"
  let pwd' = _textInput_value pwd
  alternateWebSocket obj startTime addr' pwd' toSend
  -- resettingWebSocket addr' pwd' toSend

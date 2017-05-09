{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Protocol.Foreign where

import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import qualified GHCJS.Marshal.Pure as P
import JavaScript.Object.Internal as O
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure

foreign import javascript unsafe
  "__debugEstuaryProtocol = new EstuaryProtocol(); $r = __debugEstuaryProtocol"
  estuaryProtocolFFI :: IO T.JSVal

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrlFFI :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$1.send($2)"
  sendFFI :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = $1.getTextEdit($2)"
  getTextEditFFI :: T.JSVal -> T.JSVal -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.getEstuaryEdit($2)"
  getEstuaryEditFFI :: T.JSVal -> T.JSVal -> IO T.JSVal

data EstuaryProtocolObject = EstuaryProtocolObject T.JSVal

estuaryProtocol :: IO EstuaryProtocolObject
estuaryProtocol = do
  x <- estuaryProtocolFFI
  return $ EstuaryProtocolObject x

setUrl :: EstuaryProtocolObject -> String -> IO ()
setUrl (EstuaryProtocolObject x) url = setUrlFFI x (Prim.toJSString url)

send :: EstuaryProtocolObject -> String -> IO ()
send (EstuaryProtocolObject x) y = sendFFI x (Prim.toJSString y)

getTextEdit :: EstuaryProtocolObject -> Int -> IO String
getTextEdit (EstuaryProtocolObject x) n = Prim.fromJSString (getTextEditFFI x n)

getEstuaryEdit :: EstuaryProtocolObject -> Int -> IO String
getEstuaryEdit (EstuaryProtocolObject x) n = Prim.fromJSString (getEstuaryEditFFI x n)




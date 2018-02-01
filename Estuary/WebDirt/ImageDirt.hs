{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.ImageDirt (ImageDirt(..), playSample) where
import qualified Sound.Tidal.Context as Tidal
import Data.Map
import Data.Maybe
import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import qualified GHCJS.Marshal.Pure as P
import JavaScript.Object.Internal as O
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure
import Estuary.WebDirt.Foreign (createObjFromMap)

data ImageDirt = ImageDirt (MVar [(Double,Tidal.ParamMap)])

imageDirt :: IO ImageDirt
imageDirt = do
  mv <- newMVar []
  return $ ImageDirt mv

playSample :: ImageDirt -> (Double,Tidal.ParamMap) -> IO ()
playSample (ImageDirt mv) (t,e) = do
  xs <- takeMVar mv
  putMVar mv ((t,e):xs)

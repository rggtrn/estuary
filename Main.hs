
module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.StackedPatterns
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Estuary.Widgets.Text
import Control.Monad (liftM)
import Sound.Tidal.Context (ParamPattern)
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt
import Data.Map
import Control.Monad.IO.Class (liftIO)
import Text.JSON
import Estuary.Types.EditAction

{-
main :: IO ()
main = do
  wd <- webDirt
  stream <- webDirtStream wd
  mainWidget $ do
    divClass "estuary" $ do
      newPage <- header
      divClass "page" $ do
        let firstPage = snd (pages!!0)
        let newPage' = fmap (snd . (pages !!)) newPage
        w <- widgetHold firstPage newPage'
        p <- liftM (joinDyn) $ mapDyn (fst) w
        h <- liftM (switchPromptlyDyn) $ mapDyn (snd) w
        let patternEval = updated p
        performEvent_ $ fmap (liftIO . (doHint wd)) h
        performEvent_ $ fmap (liftIO . stream) patternEval

header :: (MonadWidget t m) => m (Event t Int)
header = divClass "header" $ do
  divClass "logo" $ text "estuary (a work in progress)"
  divClass "webDirt" $ text " "
  newPageIndex <- divClass "pageMenu" $ do
    let pageNames = Prelude.map (fst) pages
    let pageList = zipWith (\x y -> (y,x)) pageNames ([0..]::[Int])
    let pageMap = constDyn $ fromList pageList
    menu <- dropdown 0 pageMap def
    return $ _dropdown_change menu
  divClass "hintArea" $ text " "
  return newPageIndex

widgetToPage :: (MonadWidget t m,ParamPatternable p) => m (Dynamic t (p,a,Event t Hint)) -> m (Dynamic t ParamPattern,Event t Hint)
widgetToPage w = do
  x <- w
  p <- mapDyn (\(a,_,_) -> toParamPattern a) x
  h <- liftM (switchPromptlyDyn) $ mapDyn (\(_,_,a) -> a) x
  return (p,h)

-- pages :: MonadWidget t m => [(String,m (Dynamic t ParamPattern,Event t Hint))]
pages = [
  ("Simple Fixed (s,vowel,up)",widgetToPage $ P.simpleFixedInterface EmptyTransformedPattern never),
  ("Text-Only Fixed (s,n,up,vowel)",widgetToPage $ textInterface EmptyTransformedPattern never),
  ("Two Stacked Patterns with Liveness controls",widgetToPage $ twoStackedPatterns)
  ]
-}

{-
topLevelTransformedPatternWidget :: MonadWidget t m =>
  Event t TransformedPattern -> -- deltas from network (must not re-propagate as edit events!)
  m (
    Dynamic t TransformedPattern, -- value for local WebDirt playback
    Event t TransformedPattern, -- deltas to network (not based on events received from network!)
    Event t Hint, -- hints (currently for WebDirt sample loading only)
  )

topLevelTransformedPatternWidget updateEvent = do
  w <- widgetHold (midLevelTransformedPatternWidget EmptyTransformedPattern) (fmap midLevelTransformedPatternWidget updateEvent)
  x <- mapDyn (\(a,_,_) -> a) w
  y <- mapDyn (\(_,a,_) -> a) w
  z <- mapDyn (\(_,_,a) -> a) w
  let x' = joinDyn x
  let y' = switchPromptlyDyn y
  let z' = switchPromptlyDyn z
  return (x',y',z')

midLevelTransformedPatternWidget :: MonadWidget t m =>
  TransformedPattern -> m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
-- i.e. adapting from what we need at higher level to recursively-structured transformedPatternWidget
-}



trivialPatternA = UntransformedPattern (S (Atom "bd" Inert Once))

trivialPatternB = UntransformedPattern (S (Atom "cp" Inert Once))

trivialTransformedPatternWidget :: MonadWidget t m => Event t TransformedPattern -> m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
trivialTransformedPatternWidget _ = el "div" $ do
  a <- liftM (trivialPatternA <$) $ button "trivialA"
  b <- liftM (trivialPatternB <$) $ button "trivialB"
  value <- holdDyn EmptyTransformedPattern $ leftmost [a,b]
  let edits = leftmost [a,b]
  return (value,edits,never)

textWidget :: MonadWidget t m => Event t String -> m (Dynamic t String,Event t (EditAction String),Event t Hint)
textWidget delta = el "div" $ do
  y <- textInput $ def & textInputConfig_setValue .~ delta
  let edits = fmap EditAction $ updated $ _textInput_value y
  evals <- liftM (EvalAction <$) $ button "eval"
  let editActions = leftmost [edits,evals]
  value <- holdDyn "" $ updated $ _textInput_value y
  return (value,editActions,never)

examplePage :: MonadWidget t m => Event t (Map Int (Either TransformedPattern String))
  -> m
    (Dynamic t (Map Int (Either TransformedPattern String)), -- values for local use
     Event t (Map Int (Either (EditAction TransformedPattern) (EditAction String))), -- edit events for broadcast
     Event t Hint) -- hint events for local use
examplePage _ = do
  -- let deltaA = fmapMaybe (either Just (const Nothing)) $ fmapMaybe (lookup 1) deltasDown
  -- let deltaB = fmapMaybe (either (const Nothing) Just) $ fmapMaybe (lookup 2) deltasDown
  test <- liftM ("test"  <$) $ button "edit the text from elsewhere"
  (aValue,aEdits,aHints) <- trivialTransformedPatternWidget never
  (bValue,bEdits,bHints) <- textWidget test
  aValue' <- mapDyn (singleton 1 . Left) aValue
  bValue' <- mapDyn (singleton 2 . Right) bValue
  values <- combineDyn (union) aValue' bValue'
  let aDeltaUp = fmap (singleton 1 . Left . EditAction) $ aEdits
  let bDeltaUp = fmap (singleton 2 . Right) $ bEdits
  let deltasUp = mergeWith union [aDeltaUp,bDeltaUp]
  -- let hintsUp = leftmost [aHints,bHints]
  return (values,deltasUp,never)

main :: IO ()
main = mainWidget $ divClass "header" $ do
  (values,deltasUp,hints) <- examplePage never
  diagnostics values deltasUp hints

diagnostics :: MonadWidget t m =>
  Dynamic t (Map Int (Either TransformedPattern String)) ->
  Event t (Map Int (Either (EditAction TransformedPattern) (EditAction String))) ->
  Event t Hint ->
  m ()
diagnostics values deltas hints = do
  el "div" $ do
    text "Values:"
    mapDyn encode values >>= display
  el "div" $ do
    text "Deltas:"
    (holdDyn "" $ fmap encode deltas) >>= display
  el "div" $ do
    text "Hints:"
    (holdDyn "" $ fmap show hints) >>= display

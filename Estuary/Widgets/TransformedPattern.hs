{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.TransformedPattern where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.PatternTransformer
import Estuary.Widgets.Generic
import Estuary.Reflex.Container
import Control.Monad
import Data.Map
import Data.List
import qualified Estuary.Widgets.SpecificPattern as Sp
import GHC.Real

import Text.Read
import Estuary.Reflex.Utility


dropdownPatternWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (),Event t Hint))
dropdownPatternWidget iPattern _ = do
  let paramShowList = ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cut", "cutoff", "delay","delayfeedback","delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "n", "pan", "resonance", "s", "shape", "speed", "unit","up", "vowel"] -- Map (Map k func) String
  let patternType = head $ words $ show iPattern
  let initialIndex = maybe (0) id $ Data.List.findIndex (==patternType) paramShowList -- Int of initalFunc
  let patMap = Data.Map.insert initialIndex (Sp.specificContainer iPattern never) $ fromList $ zip [0..] builderList
  let initialFunc = maybe (builderList!!0) (id) $ Data.Map.lookup initialIndex patMap
  let dropDownMap = constDyn $ fromList $ zip [0::Int,1..] paramShowList
  patternDropDown <- dropdown initialIndex dropDownMap def
  let ddVal = _dropdown_value patternDropDown
  soundPat <- mapDyn (\k ->case Data.Map.lookup k patMap of Just a-> a; otherwise -> Sp.specificContainer (S $ Blank Inert) never) ddVal  --Dynamic (m(dynamic spec,event t))
  let soundPatEv = updated soundPat -- Event(Dyn ,ev,ev)
  soundPatEv' <- widgetHold (initialFunc) soundPatEv  -- m Dynamic t(m (Dynamic (spec,event gen)...))
  let soundPattern = joinDyn soundPatEv' --Dyn (spec , event generic)
  return $ soundPattern
  where
    builderList = Prelude.map (\x-> Sp.specificContainer x never) [(Accelerate $ Atom 0 Inert Once),
      (Bandf $ Atom 440 Inert Once),
      (Bandq $ Atom 10 Inert Once),
      (Begin $ Atom 0 Inert Once),
      (Coarse $ Atom 0 Inert Once),
      (Crush $ Atom 16 Inert Once),
      (Estuary.Tidal.Types.Cut $ Atom 1 Inert Once),
      (Cutoff $ Atom 440 Inert Once),
      (Delay $ Atom 0 Inert Once),
      (Delayfeedback $ Atom 0 Inert Once),
      (Delaytime $ Atom 0.5 Inert Once),
      (End $ Atom 1 Inert Once),
      (Gain $ Atom 1 Inert Once),
      (Hcutoff $ Atom 440 Inert Once),
      (Hresonance $ Atom 20 Inert Once),
      (Loop $ Atom 0 Inert Once),
      (N $ Atom 0 Inert Once),
      (Pan $ Atom 0.5 Inert Once),
      (Resonance $ Atom 0.5 Inert Once),
      (S $ Atom "~" Inert Once),
      (Shape $ Atom 0.5 Inert Once),
      (Speed $ Atom 1 Inert Once),
      (Unit $ Atom 'c' Inert Once),
      (Up $ Atom 0 Inert Once),
      (Vowel $ Atom 'o' Inert Once)] --, stringContainerWidget (Unit $ Atom "c" Once),stringContainerWidget (Vowel $ Atom "c" Once)]


patternCombinatorDropDown :: MonadWidget t m => PatternCombinator -> Event t () -> m (Dynamic t (PatternCombinator,Event t (EditSignal a)))
patternCombinatorDropDown iValue _ = do
  let ddMapVals = fromList $ zip [(1::Int)..] [Merge,Add,Subtract,Multiply,Divide]
  let ddMap = constDyn $ fromList $ zip [(1::Int)..] $ fmap show [Merge,Add,Subtract,Multiply,Divide]
  dd <- dropdown iIndex ddMap def
  --mapDyn show (_dropdown_value dd) >>= dynText
  let choice = _dropdown_value dd
  val <- mapDyn (maybe Merge id . (flip Data.Map.lookup) ddMapVals) choice
  mapDyn (\x->(x,never)) val
  --mapDyn ((flip Data.Map.lookup) ddMapVals) choice >>= mapDyn (\x -> (x,never))
  where
    iIndex = case iValue of
      (Merge) -> 1
      (Add) -> 2
      (Subtract) -> 3
      (Multiply) -> 4
      (Divide) -> 5
      -- sorry....



resettableTransformedPatternWidget :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t(TransformedPattern, Event t (EditSignal a),Event t Hint))
resettableTransformedPatternWidget iTransPat ev = mdo
  val <- resettableWidget (transformedPat) iTransPat ev resetEvent
  tPat <- mapDyn (\(x,_,_)->x) val
  e <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_)->x) val
  h <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x)->x) val
  let resetEvent = tagDyn tPat $ ffilter (\x-> case x of RebuildMe->True; DeleteMe -> True; otherwise -> False) e
  mapDyn (\x->(x,e,h)) tPat


transformedPat :: MonadWidget t m => TransformedPattern -> Event t (EditSignal a) -> m (Dynamic t (TransformedPattern, Event t (EditSignal a), Event t Hint))
transformedPat (EmptyTransformedPattern) _ = do
  x <- liftM (UntransformedPattern (S (Atom "~" Inert Once))  <$) $ button "make me not empty"
  value <- holdDyn EmptyTransformedPattern x
  let event = (RebuildMe <$) x
  mapDyn (\y -> (y,event,never)) value
transformedPat (UntransformedPattern specificPattern) _= do
  delete <- button "delete"
  transform <- button "transform" -- >>= toggle False 
  sPatTuple <- dropdownPatternWidget specificPattern never
  sPat <- mapDyn (\(x,_,_)->x) sPatTuple
  hint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) sPatTuple
  ev <- mapDyn (\(_,ev,_)->ev) sPatTuple
  tPat <- mapDyn (UntransformedPattern) sPat
  combine <- button "+"
  let delete' = (EmptyTransformedPattern <$) delete
  let updatedValue = attachDynWith (\sp _-> constDyn $ TransformedPattern NoTransformer sp) tPat transform
  let combineValue = attachDynWith (\sp trans-> constDyn $ TransformedPattern (Combine sp Merge) $ UntransformedPattern (Speed $ Atom  1 Inert Once)) sPat combine
  let updatedValue' = leftmost [updatedValue, combineValue, fmap constDyn delete']
  value <- liftM joinDyn $ holdDyn tPat updatedValue'
  let rebuildEvents = leftmost [(DeleteMe <$) delete, (RebuildMe <$) transform, (RebuildMe <$) combine]
  holdDyn "asdfasdf" (("triggered" <$) delete) >>= dynText
  mapDyn (\x->(x,rebuildEvents,hint)) value
transformedPat (TransformedPattern (Combine iSpecPat iPatComb) iTransPat) _ = do  
  delete <- button "delete"
  transform <- button "transform"
  sPatTuple <- dropdownPatternWidget iSpecPat never
  sPat <- mapDyn (\(x,_,_)->x) sPatTuple
  sHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) sPatTuple
  (comb,_) <- patternCombinatorDropDown iPatComb never >>= splitDyn
  tPatTuple <- resettableTransformedPatternWidget iTransPat never  -- this one has to have the 'reset' wrapper around it
  tPat <- mapDyn (\(x,_,_)->x) tPatTuple
  tEv <- liftM switchPromptlyDyn $ mapDyn (\(_,ev,_)->ev) tPatTuple
  tHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h)->h) tPatTuple

  val <- combineDyn (\x y -> TransformedPattern (Combine x y)) sPat comb >>= combineDyn (\t cons -> cons t) tPat
  let childDeleteMe = ffilter (\x->case x of DeleteMe ->True; otherwise-> False)  tEv
  transVal <- mapDyn (TransformedPattern NoTransformer) val
  untransPat <- mapDyn UntransformedPattern sPat
  val' <- liftM joinDyn $ holdDyn val $ fmap (const tPat) delete 
  val''<- liftM joinDyn $ holdDyn val' $ (untransPat <$) childDeleteMe
  val''' <- liftM joinDyn $ holdDyn val'' $ (transVal <$) transform
  mapDyn (\x->(x, leftmost [(RebuildMe <$) delete,(RebuildMe <$) childDeleteMe, (RebuildMe <$) transform],leftmost [tHint,sHint])) val'''
transformedPat (TransformedPattern iPatTrans iTransPat) _ = do
  delete <- button "delete transformer"
  patTrans <- parameteredPatternTransformer iPatTrans never >>= mapDyn fst
  tPatTuple <- resettableTransformedPatternWidget iTransPat never
  transPat <- mapDyn (\(x,_,_)->x) tPatTuple
  tEv <- liftM switchPromptlyDyn $ mapDyn (\(_,ev,_)->ev) tPatTuple
  tHint <- liftM switchPromptlyDyn $ mapDyn (\(_,_,x)->x) tPatTuple
  let rebuildVal = tagDyn transPat $ leftmost [(DeleteMe <$) delete,ffilter (\x->case x of DeleteMe->True;otherwise->False) tEv]
  newTransPat<- combineDyn (\x y-> TransformedPattern x y) patTrans transPat
  val <- liftM joinDyn $ holdDyn newTransPat $ fmap constDyn rebuildVal
  mapDyn (\x-> (x,leftmost [(RebuildMe <$) delete,(RebuildMe <$) rebuildVal],tHint)) val



paramWidget::MonadWidget t m=>PatternTransformer -> m (Dynamic t PatternTransformer)
paramWidget (Jux trans) = do
  trans' <- parameteredPatternTransformer trans never
  val'<- mapDyn (\(next,_)-> Jux next) trans'
  return val'
paramWidget (Every num trans) = do

  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (show num)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  nextTrans <- parameteredPatternTransformer trans never
  val'<-combineDyn (\k (next,_)-> Every k next) val nextTrans
  return val'
paramWidget (Slow i) = do
  let numer = numerator i
  let denom = denominator i
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (show numer)
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (show denom)
  let input2' = _textInput_value input2 -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Integer))
  val2 <- forDyn input2' (\x-> maybe 1 id (readMaybe x::Maybe Integer))
  combineDyn (\x y->  Slow ((x%y)::Rational)) val val2
paramWidget (Density i)= do
  let numer = numerator i
  let denom = denominator i
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"]))  & textInputConfig_initialValue .~ (show numer)
  let input' = _textInput_value input -- Dyn string
  input2 <- textInput $ def & textInputConfig_attributes .~ (constDyn (fromList $ zip ["type","style"] ["number","width:30px"])) & textInputConfig_initialValue .~ (show denom)
  let input2' = _textInput_value input2 -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Integer))
  val2 <- forDyn input2' (\x-> maybe 1 id (readMaybe x::Maybe Integer))
  combineDyn (\x y-> Density $ (x%y::Rational) ) val val2
paramWidget (DegradeBy i) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (show i)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Double))
  val'<-forDyn val (\k-> DegradeBy k)
  return val'
paramWidget (Chop i) = do
  input <- textInput $ def & textInputConfig_attributes .~ (constDyn ("type"=:"number")) & textInputConfig_initialValue .~ (show i)
  let input' = _textInput_value input -- Dyn string
  val <- forDyn input' (\x->maybe 1 id (readMaybe x::Maybe Int))
  val'<-forDyn val (\k-> Chop k)
  return val'
paramWidget (Combine iSPat iPatComb) = do
  sPat <- dropdownPatternWidget iSPat never >>= mapDyn (\(x,_,_)->x)
  comb <- patternCombinatorDropDown iPatComb never >>= mapDyn fst
  combineDyn Combine sPat comb
paramWidget transformer = return $ constDyn transformer

parameteredPatternTransformer::MonadWidget t m => PatternTransformer -> Event t () -> m (Dynamic t (PatternTransformer, ()))
parameteredPatternTransformer i _ = el "div" $ do
  let transMap = fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] [NoTransformer,Rev,Slow 1, Density 1, Degrade, DegradeBy 0.5, Brak,Every 1 NoTransformer, Jux NoTransformer, Chop 1,Combine (Speed $ Atom 1 Inert Once) Multiply]
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6,7,8,9,10] ["NoTransformer","Rev","Slow","Density", "Degrade", "DegradeBy","Brak","Every","Jux","Chop","Combine"]
  dd <- dropdown (hack i) ddMap def
  let ddVal = _dropdown_value dd -- Dynamic int
  ddWidget <- mapDyn (\k ->case Data.Map.lookup k transMap of Just a->paramWidget a; otherwise -> paramWidget NoTransformer) ddVal  --Dynamic (m(dynamic transformer))
  let ddWidgetValEv =  updated ddWidget -- Event (M (dyn Pattern))
  paramValue <- widgetHold (paramWidget i) ddWidgetValEv  -- m Dynamic t(m (Dynamic PatternTrans...))
  let paramValue' = joinDyn paramValue --Dyn PatternTransformer
  mapDyn (\k->(k,())) paramValue'
  where
    hack NoTransformer = 0
    hack Rev = 1
    hack (Slow _) = 2 -- sorry...
    hack (Density _) = 3
    hack Degrade = 4
    hack (DegradeBy _) = 5
    hack Brak = 6
    hack (Every _ _)= 7
    hack (Jux _) = 8
    hack (Chop _) = 9
    hack (Combine _ _) = 10
    hack _ = 0




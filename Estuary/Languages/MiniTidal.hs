module Estuary.Languages.MiniTidal (miniTidalPattern) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal

miniTidalPattern :: String -> Tidal.ParamPattern
miniTidalPattern x = either (const Tidal.silence) id $ parse miniTidalParser "(unknown)" x

miniTidalParser :: GenParser Char a Tidal.ParamPattern
miniTidalParser = spaces >> patternOrTransformedPattern

patternOrTransformedPattern :: GenParser Char a (Tidal.ParamPattern)
patternOrTransformedPattern = choice [
  try transformedPattern0,
  try transformedPattern1,
  try transformedPattern2,
  specificPattern
  ]

transformedPattern0 :: GenParser Char a (Tidal.ParamPattern)
transformedPattern0 = do
  x <- specificPattern
  m <- mergeOperator
  y <- patternOrTransformedPattern
  spaces
  return $ m x y

transformedPattern1 :: GenParser Char a (Tidal.ParamPattern)
transformedPattern1 = do
  x <- patternTransformation
  char '$'
  spaces
  y <- patternOrTransformedPattern
  spaces
  return $ x y

transformedPattern2 :: GenParser Char a (Tidal.ParamPattern)
transformedPattern2 = do
  x <- patternTransformation
  char '('
  spaces
  y <- patternOrTransformedPattern
  spaces
  char ')'
  return $ x y

patternTransformation :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
patternTransformation = do
  spaces
  x <- choice [ try patternTransformationInBrackets, patternTransformations]
  spaces
  return x

patternTransformationInBrackets :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
patternTransformationInBrackets = do
  char '('
  spaces
  x <- patternTransformations
  spaces
  char ')'
  spaces
  return x

patternTransformations :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
patternTransformations = choice [
  try mergedPattern,
  try (string "brak" >> return Tidal.brak),
  try (string "rev" >> return Tidal.rev),
  try (string "fast" >> spaces >> fractional3 False >>= return . Tidal.fast),
  try (string "slow" >> spaces >> fractional3 False >>= return . Tidal.slow),
  try append,
  string "jux" >> spaces >> patternTransformation >>= return . Tidal.jux
  ]

append :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
append = do
  string "append" >> spaces
  x <- patternOrTransformedPattern
  spaces
  return $ Tidal.append x

mergedPattern :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
mergedPattern = do
  x <- specificPattern
  spaces
  m <- mergeOperator
  return $ m x

mergeOperator :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern -> Tidal.ParamPattern)
mergeOperator = char '#' >> spaces >> return (Tidal.#)

specificPattern :: GenParser Char a (Tidal.ParamPattern)
specificPattern = choice [
  try (string "s" >> spaces >> genericPattern >>= return . Tidal.s),
  try (string "n" >> spaces >> genericPattern >>= return . Tidal.n),
  try (string "up" >> spaces >> genericPattern >>= return . Tidal.up),
  try (string "vowel" >> spaces >> genericPattern >>= return . Tidal.vowel),
  try (string "pan" >> spaces >> genericPattern >>= return . Tidal.pan),
  try (string "shape" >> spaces >> genericPattern >>= return . Tidal.shape)
  ]

genericPattern :: Tidal.Parseable b => GenParser Char a (Tidal.Pattern b)
genericPattern = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  spaces
  return $ Tidal.p x

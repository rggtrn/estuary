{-# LANGUAGE OverloadedStrings #-}

module MiniTidalTests where

import Test.Hspec
import Estuary.Languages.MiniTidal
import Sound.Tidal.Context as Tidal
import Data.Either

parsesTo :: String -> ParamPattern -> Expectation
parsesTo s p = x `shouldBe` y
  where x = arc <$> miniTidalParser s <*> Right (0,16)
        y = Right $ arc p $ (0,16)

causesParseError :: String -> Expectation
causesParseError s = isLeft (miniTidalParser s) `shouldBe` True

main :: IO ()
main = hspec $ do

  describe "miniTidalParser" $ do

    it "parses the empty string as silence" $
      "" `parsesTo` silence

    it "parses a string containing only spaces as silence" $
      "    " `parsesTo` silence

    it "parses a single 's' pattern" $
      "s \"bd cp\"" `parsesTo` s "bd cp"

    it "parses two merged patterns" $
      "s \"bd cp\" # pan \"0 1\"" `parsesTo` (s "bd cp" # pan "0 1")

    it "parses three merged patterns" $
      "s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\"" `parsesTo`
        (s "bd cp" # pan "0 1" # gain "0.5 0.7")

    it "parses three merged patterns, everything in brackets" $
      "(s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\")" `parsesTo`
        ((s "bd cp" # pan "0 1" # gain "0.5 0.7"))

    it "parses three merged patterns, everything in muliple layers of brackets" $
      "(((s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\")))" `parsesTo`
        ((((s "bd cp" # pan "0 1" # gain "0.5 0.7"))))

    it "parses three merged patterns with right associative brackets" $
      "s \"bd cp\" # (pan \"0 1\" # gain \"0.5 0.7\")" `parsesTo`
        (s "bd cp" # (pan "0 1" # gain "0.5 0.7"))

    it "parses three merged patterns with left associative brackets" $
      "(s \"bd cp\" # pan \"0 1\") # gain \"0.5 0.7\"" `parsesTo`
        ((s "bd cp" # pan "0 1") # gain "0.5 0.7")

    it "parses simple patterns in brackets applied to ParamPattern functions" $
      "s (\"bd cp\")" `parsesTo` (s ("bd cp"))

    it "parses simple patterns applied to ParamPattern functions with $" $
      "s $ \"bd cp\"" `parsesTo` (s $ "bd cp")

    it "parses addition of simple patterns" $
      "n (\"0 1\" + \"2 3\")" `parsesTo` (n ("0 1" + "2 3"))

    it "parses multiplication of simple patterns as a merged parampattern" $
      "s \"arpy*8\" # up (\"3\" * \"2\")" `parsesTo` (s "arpy*8" # up ("3" * "2"))

    it "parses pan patterns" $
      "pan \"0 0.25 0.5 0.75 1\"" `parsesTo` (pan "0 0.25 0.5 0.75 1")

    it "parses sinewave oscillators" $
      "pan sinewave" `parsesTo` (pan sinewave)

    it "parses sinewave1 oscillators" $
      "pan sinewave1" `parsesTo` (pan sinewave1)

    it "parses sinewave1 oscillators used in pan patterns" $
      "s \"arpy*8\" # pan sinewave1" `parsesTo` (s "arpy*8" # pan sinewave1)

    it "parses fast transformations of parampatterns" $
      "fast 2 $ s \"bd cp\"" `parsesTo` (fast 2 $ s "bd cp")

    it "parses fast transformations of parampatterns when in brackets" $
      "(fast 2) $ s \"bd cp\"" `parsesTo` ((fast 2) $ s "bd cp")

    it "parses rev transformations of parampatterns" $
      "rev $ s \"bd cp\"" `parsesTo` (rev $ s "bd cp")

    it "parses rev transformations of parampatterns when in brackets" $
      "(rev) $ s \"bd cp\"" `parsesTo` ((rev) $ s "bd cp")

    it "parses jux transformations with transformations in brackets" $
        "jux (rev) $ s \"arpy*8\" # up \"0 2 3 5 3 5 7 8\"" `parsesTo`
         (jux (rev) $ s "arpy*8" # up "0 2 3 5 3 5 7 8")

    it "parses jux transformations with transformations not in brackets" $
        "jux rev $ s \"arpy*8\" # up \"0 2 3 5 3 5 7 8\"" `parsesTo`
         (jux rev $ s "arpy*8" # up "0 2 3 5 3 5 7 8")

    it "doesn't parse when a transformation requiring an argument is provided without parens or $ to jux" $
      causesParseError "jux fast 2 $ s \"bd*4 cp\""

    it "parses multiple fast transformations of parampatterns" $
      "fast 2 $ fast 2 $ s \"bd cp\"" `parsesTo` (fast 2 $ fast 2 $ s "bd cp")

    it "parses an 'every' transformation applied to a simple s pattern" $
      "every 2 (fast 2) (s \"bd cp\")" `parsesTo` (every 2 (fast 2) (s "bd cp"))

    it "parses a transformed pattern merged with a pattern constructed from parampatterning an arithmetic expression on patterns" $
      "(every 2 (fast 2) $ s \"arpy*8\") # up (\"[0 4 7 2,16 12 12 16]\" - \"<0 3 5 7>\")" `parsesTo` ((every 2 (fast 2) $ s "arpy*8") # up ("[0 4 7 2,16 12 12 16]" - "<0 3 5 7>"))

    it "parses a fast transformation applied to a simple (ie. non-param) pattern" $
      "up (fast 2 \"<0 2 3 5>\")" `parsesTo`
        (up (fast 2 "<0 2 3 5>"))

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}

module Main (main) where

import Lens.Micro
  ( Lens', lens, set )
import Lens.Micro.Extras
  ( view )
import Taiwan.ID
  ( ID (..) )
import Taiwan.ID.CharIndex
  ( CharIndex (CharIndex) )
import Taiwan.ID.CharSet
  ( CharSet (..) )
import Taiwan.ID.Digit
  ( Digit (..) )
import Taiwan.ID.Digit1289
  ( Digit1289 (..) )
import Taiwan.ID.Gender
  ( Gender (..) )
import Taiwan.ID.Issuer
  ( Issuer (..) )
import Taiwan.ID.Language
  ( Language (..) )
import Taiwan.ID.Letter
  ( Letter (..) )
import Taiwan.ID.Region
  ( Region )
import Taiwan.ID.Test
  ( unsafePokeChar )
import Test.Hspec
  ( Spec, describe, hspec, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
  ( Arbitrary (..)
  , NonEmptyList (..)
  , Property
  , choose
  , elements
  , forAll
  , property
  , (===)
  )
import Test.QuickCheck.Classes
  ( boundedEnumLaws, eqLaws, numLaws, ordLaws, showLaws, showReadLaws )
import Test.Hspec.QuickCheck.Classes
  ( testLaws )

import qualified Data.Finitary as Finitary
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified Taiwan.ID as ID
import qualified Taiwan.ID.Test as Test

instance Arbitrary Digit where
  arbitrary = Test.genDigit
  shrink = Test.shrinkDigit

instance Arbitrary Digit1289 where
  arbitrary = Test.genDigit1289
  shrink = Test.shrinkDigit1289

instance Arbitrary Gender where
  arbitrary = Test.genGender
  shrink = Test.shrinkGender

instance Arbitrary ID where
  arbitrary = Test.genID
  shrink = Test.shrinkID

instance Arbitrary Issuer where
  arbitrary = Test.genIssuer
  shrink = Test.shrinkIssuer

instance Arbitrary Language where
  arbitrary = Test.genLanguage
  shrink = Test.shrinkLanguage

instance Arbitrary Letter where
  arbitrary = Test.genLetter
  shrink = Test.shrinkLetter

instance Arbitrary Region where
  arbitrary = Test.genRegion
  shrink = Test.shrinkRegion

main :: IO ()
main = hspec $ do

  describe "Class laws" $ do

    testLaws @Digit
        [ boundedEnumLaws
        , eqLaws
        , numLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @Digit1289
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @Gender
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @ID
        [ eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @Issuer
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @Language
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @Letter
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLaws @Region
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

  describe "Finitary instances" $ do

    describe "ID" $ do

      let start    = Finitary.start    @ID
          end      = Finitary.end      @ID
          previous = Finitary.previous @ID
          next     = Finitary.next     @ID

      it "previous start" $
          previous start
            `shouldBe` Nothing

      it "start" $
          start
            `shouldBe` ID.fromSymbol @"A100000001"

      it "next start" $
          next start
            `shouldBe` Just (ID.fromSymbol @"A100000010")

      it "previous end" $
          previous end
            `shouldBe` Just (ID.fromSymbol @"Z999999987")

      it "end" $
          end
            `shouldBe` ID.fromSymbol @"Z999999996"

      it "next end" $
          next end
            `shouldBe` Nothing

  describe "ID attribute getters and setters" $ do

    describe "Gender" $
      checkLensLaws gender
    describe "Issuer" $
      checkLensLaws issuer
    describe "Region" $
      checkLensLaws region

  describe "ID.fromText" $ do

    it "successfully parses known-valid identification numbers" $
      forAll (elements knownValidIDs) $ \i ->
      ID.fromText (ID.toText i) `shouldBe` Right i

    it "successfully parses valid identification numbers" $
      property $
      forAll Test.genID $ \validID ->
      ID.fromText (ID.toText validID) `shouldBe` Right validID

    it "does not parse identification numbers that are too short" $
      property $
      forAll Test.genIDTextInvalidLengthTooShort $ \invalidID ->
      ID.fromText invalidID `shouldBe` Left ID.InvalidLength

    it "does not parse identification numbers that are too long" $
      property $
      forAll Test.genIDTextInvalidLengthTooLong $ \invalidID ->
      ID.fromText invalidID `shouldBe` Left ID.InvalidLength

    it "does not parse identification numbers with invalid region codes" $
      property $
      forAll (Test.genIDTextInvalidCharAtIndex 0) $ \invalidID ->
      ID.fromText invalidID `shouldBe` Left
        (ID.InvalidChar 0 (CharRange 'A' 'Z'))

    it "does not parse identification numbers with invalid initial digits" $
      property $
      forAll (Test.genIDTextInvalidCharAtIndex 1) $ \invalidID ->
      ID.fromText invalidID `shouldBe` Left
        (ID.InvalidChar 1 (CharSet $ NESet.fromList ['1', '2', '8', '9']))

    it "does not parse identification numbers with invalid serial digits" $
      property $
      forAll (choose (2, 9)) $ \invalidCharIndex ->
      forAll (Test.genIDTextInvalidCharAtIndex invalidCharIndex) $ \invalidID ->
      ID.fromText invalidID `shouldBe` Left
        (ID.InvalidChar (CharIndex invalidCharIndex) (CharRange '0' '9'))

    it "does not parse identification numbers with invalid checksums" $
      property $
      forAll Test.genIDTextInvalidChecksum $ \invalidID ->
      ID.fromText invalidID `shouldBe` Left ID.InvalidChecksum

    it "reports invalid characters even when input is too short" $
      property $ \(i :: ID) ->
      forAll Test.genInvalidChar $ \invalidChar ->
      forAll (choose (1, 9)) $ \truncatedLength ->
      forAll (choose (0, truncatedLength - 1)) $ \invalidCharIndex -> do
      let truncatedID = T.take truncatedLength (ID.toText i)
      let invalidID = unsafePokeChar invalidCharIndex truncatedID invalidChar
      ID.fromText invalidID `shouldSatisfy` \case
        Left (ID.InvalidChar (CharIndex index) _)
          | index == invalidCharIndex -> True
        _ -> False

    it "does not report invalid characters if input is too long" $
      property $ \(NonEmpty trailingExcess) ->
      forAll Test.genIDTextInvalidChar $ \invalidID ->
      ID.fromText (invalidID <> T.pack trailingExcess) `shouldBe`
        Left ID.InvalidLength

checkLensLaws
  :: forall i v. (Arbitrary i, Arbitrary v, Eq i, Eq v, Show i, Show v)
  => Lens' i v
  -> Spec
checkLensLaws l =
  do
    it "Finality"
      $ property lensLawFinality
    it "Idempotence"
      $ property lensLawIdempotence
    it "Invertibility"
      $ property lensLawInvertibility
    it "Reversibility"
      $ property lensLawReversibility
    it "Stability"
      $ property lensLawStability
  where
    lensLawFinality :: i -> v -> v -> Property
    lensLawFinality i v1 v2 = set l v2 (set l v1 i) === set l v2 i

    lensLawIdempotence :: i -> v -> Property
    lensLawIdempotence i v = set l v (set l v i) === set l v i

    lensLawInvertibility :: i -> v -> Property
    lensLawInvertibility i v = view l (set l v i) === v

    lensLawReversibility :: i -> v -> Property
    lensLawReversibility i v = set l (view l i) (set l v i) === i

    lensLawStability :: i -> Property
    lensLawStability i = set l (view l i) i === i

gender :: Lens' ID Gender
gender = lens ID.getGender (flip ID.setGender)

issuer :: Lens' ID Issuer
issuer = lens ID.getIssuer (flip ID.setIssuer)

region :: Lens' ID Region
region = lens ID.getRegion (flip ID.setRegion)

-- | A set of known-valid ID numbers.
--
-- Generated with 身分證字號產生器.
--
-- See: https://www.csie.ntu.edu.tw/~b90057/use/ROCid.html
--
knownValidIDs :: [ID]
knownValidIDs =
  [ ID.fromSymbol @"A123961383"
  , ID.fromSymbol @"B210742224"
  , ID.fromSymbol @"C120930548"
  , ID.fromSymbol @"D257991149"
  , ID.fromSymbol @"E127379116"
  , ID.fromSymbol @"F235628112"
  , ID.fromSymbol @"G105851924"
  , ID.fromSymbol @"H247910878"
  , ID.fromSymbol @"I118949082"
  , ID.fromSymbol @"J218475156"
  , ID.fromSymbol @"K150252170"
  , ID.fromSymbol @"L298479266"
  , ID.fromSymbol @"M114415878"
  , ID.fromSymbol @"N242846162"
  , ID.fromSymbol @"O184333688"
  , ID.fromSymbol @"P257366789"
  , ID.fromSymbol @"Q163999855"
  , ID.fromSymbol @"R275744925"
  , ID.fromSymbol @"S158047168"
  , ID.fromSymbol @"T296696104"
  , ID.fromSymbol @"U108929984"
  , ID.fromSymbol @"V245356279"
  , ID.fromSymbol @"W127612989"
  , ID.fromSymbol @"X234128072"
  , ID.fromSymbol @"Y140531128"
  , ID.fromSymbol @"Z250358466"
  ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}

module Main where

import Data.Bifunctor
  ( Bifunctor (second) )
import Data.Char
  ( intToDigit )
import Data.List.NonEmpty
  ( NonEmpty ((:|)) )
import Data.Text
  ( Text )
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
import Taiwan.ID.Letter
  ( Letter (..) )
import Taiwan.ID.Location
  ( Location )
import Test.Hspec
  ( Spec, describe, hspec, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
  ( Arbitrary (..)
  , NonEmptyList (..)
  , Property
  , arbitraryBoundedEnum
  , choose
  , elements
  , forAll
  , property
  , shrinkBoundedEnum
  , shrinkMap
  , (===)
  )
import Test.QuickCheck.Classes
  ( boundedEnumLaws, eqLaws, numLaws, ordLaws, showLaws, showReadLaws )
import Test.QuickCheck.Classes.Hspec
  ( testLawsMany )

import qualified Data.Finitary as Finitary
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified Taiwan.ID as ID

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Digit1289 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Gender where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary ID where
  arbitrary = idFromTuple <$> arbitrary
  shrink = shrinkMap idFromTuple idToTuple

instance Arbitrary Issuer where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Letter where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Location where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

main :: IO ()
main = hspec $ do

  describe "Class laws" $ do

    testLawsMany @Digit
        [ boundedEnumLaws
        , eqLaws
        , numLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @Gender
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @ID
        [ eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @Issuer
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @Location
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
    describe "Location" $
      checkLensLaws location

  describe "ID.fromText" $ do

    it "successfully parses known-valid identification numbers" $
      forAll (elements knownValidIDs) $ \i ->
        ID.fromText (ID.toText i) `shouldBe` Right i

    it "successfully parses valid identification numbers" $
      property $ \(i :: ID) ->
        ID.fromText (ID.toText i) `shouldBe` Right i

    it "does not parse identification numbers that are too short" $
      property $ \(i :: ID) n -> do
        let newLength = n `mod` 10
        let invalidID = T.take newLength $ ID.toText i
        ID.fromText invalidID `shouldBe` Left ID.InvalidLength

    it "does not parse identification numbers that are too long" $
      property $ \(i :: ID) (NonEmpty s) -> do
        let invalidID = ID.toText i <> T.pack s
        ID.fromText invalidID `shouldBe` Left ID.InvalidLength

    it "does not parse identification numbers with invalid location codes" $
      property $ \(i :: ID) (c :: Int) -> do
        let invalidLocationCode = intToDigit $ c `mod` 10
        let invalidID = replaceCharAt 0 invalidLocationCode $ ID.toText i
        ID.fromText invalidID `shouldBe`
          Left (ID.InvalidChar 0 (CharRange 'A' 'Z'))

    it "does not parse identification numbers with invalid initial digits" $
      property $ \(i :: ID) ->
        forAll (elements ['0', '3', '4', '5', '6', '7']) $ \initialDigit -> do
          let invalidID = replaceCharAt 1 initialDigit (ID.toText i)
          let expectedError =
                ID.InvalidChar 1
                  (CharSet $ NESet.fromList $ '1' :| ['2', '8', '9'])
          ID.fromText invalidID `shouldBe` Left expectedError

    it "does not parse identification numbers with invalid checksums" $
      property $ \(i :: ID) (c :: Int) -> do
        let invalidChecksumDigit = intToDigit $
              ((c `mod` 9) + fromEnum (ID.checksum i) + 1) `mod` 10
        let invalidID =
              T.take 9 (ID.toText i) <> T.pack [invalidChecksumDigit]
        ID.fromText invalidID `shouldBe` Left ID.InvalidChecksum

    it "reports invalid characters even when input is too short" $
      property $ \(i :: ID) ->
      forAll (choose (1, 9)) $ \truncatedLength ->
      forAll (choose (0, truncatedLength - 1)) $ \invalidCharIndex -> do
        let textTruncated = T.take truncatedLength (ID.toText i)
        let textInvalid = replaceCharAt invalidCharIndex 'x' textTruncated
        ID.fromText textInvalid `shouldSatisfy` \case
          Left (ID.InvalidChar (CharIndex index) _)
            | index == invalidCharIndex -> True
          _ -> False

    it "does not report invalid characters if input is too long" $
      property $ \(i :: ID) (NonEmpty trailingExcess) ->
      forAll (choose (0, 9)) $ \invalidCharIndex -> do
        let textInvalid =
              replaceCharAt invalidCharIndex 'x' (ID.toText i)
              <>
              T.pack trailingExcess
        ID.fromText textInvalid `shouldBe` Left ID.InvalidLength

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

location :: Lens' ID Location
location = lens ID.getLocation (flip ID.setLocation)

-- | Replaces a character at a specific position.
--
replaceCharAt :: Int -> Char -> Text -> Text
replaceCharAt i c t
    | i < 0 || i >= T.length t = error "replaceCharAt: invalid index"
    | otherwise = prefix <> T.singleton c <> suffix
  where
    (prefix, suffix) = second (T.drop 1) (T.splitAt i t)

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

idFromTuple :: Digit ~ d => (Letter, Digit1289, d, d, d, d, d, d, d) -> ID
idFromTuple (x0, x1, x2, x3, x4, x5, x6, x7, x8) =
  ID x0 x1 x2 x3 x4 x5 x6 x7 x8

idToTuple :: Digit ~ d => ID -> (Letter, Digit1289, d, d, d, d, d, d, d)
idToTuple (ID x0 x1 x2 x3 x4 x5 x6 x7 x8) =
  (x0, x1, x2, x3, x4, x5, x6, x7, x8)

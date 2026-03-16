{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Functor law" -}

module Main where

import Control.Monad
  ( replicateM
  )
import Control.Monad.Random
  ( evalRand
  )
import Data.Functor.Identity
  ( Identity (Identity, runIdentity)
  )
import Data.Text
  ( Text
  )
import GHC.Stack
  ( HasCallStack
  )
import System.FilePath
  ( (<.>)
  , (</>)
  )
import System.Random
  ( mkStdGen
  )
import Taiwan.ID
  ( ID (ID)
  )
import Taiwan.ID.CLI
  ( Command (Decode, Generate, Validate)
  , CommandLineResult (CommandLineFailure, CommandLineSuccess)
  , DecodeCommand (DecodeCommand, language)
  , GenerateCommand (GenerateCommand, count, seed)
  , Stage (Raw)
  , ValidateCommand (ValidateCommand, idText)
  )
import Taiwan.ID.Digit
  ( Digit
  )
import Taiwan.ID.Digit1289
  ( Digit1289
  )
import Taiwan.ID.Language
  ( Language
  )
import Taiwan.ID.Letter
  ( Letter
  )
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink)
  , Arbitrary1 (liftArbitrary)
  , Gen
  , arbitraryBoundedEnum
  , choose
  , elements
  , oneof
  , shrinkBoundedEnum
  , shrinkMap
  , vectorOf
  )
import Test.QuickCheck.Gen
  ( unGen
  )
import Test.QuickCheck.Random
  ( mkQCGen
  )
import Test.Tasty
  ( TestTree
  , defaultMain
  , testGroup
  )
import Test.Tasty.Golden
  ( goldenVsString
  )
import Text.Printf
  ( printf
  )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Taiwan.ID as ID
import qualified Taiwan.ID.CLI as CLI
import qualified Taiwan.ID.Digit as Digit

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

testRootDirectory :: FilePath
testRootDirectory = "data" </> "taiwan-id-cli-test"

testSeed :: Int
testSeed = 0

-- | The number of times to invoke each command with random arguments.
invocationsPerCommand :: Int
invocationsPerCommand = 1000

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "CLI"
      [ makeTestTree
          "decode"
          genCommandDecode
      , makeTestTree
          "generate"
          genCommandGenerate
      , makeTestTree
          "validate"
          genCommandValidate
      ]

makeTestTree :: String -> Gen CommandInvocation -> TestTree
makeTestTree commandName genInvocation =
  testGroup commandName $
    [ makeTest commandName i invocation
    | (i, invocation) <- zip [0 ..] invocations
    ]
  where
    invocations :: [CommandInvocation]
    invocations =
      unGen
        (vectorOf invocationsPerCommand genInvocation)
        (mkQCGen testSeed)
        10

makeTest :: String -> Int -> CommandInvocation -> TestTree
makeTest commandName invocationIndex invocation =
  goldenVsString testName testFilePath (pure testFileContent)
  where
    testName :: String
    testName = case invocation of
      CommandInvocation {command, optionStyle} ->
        "taiwan-id "
          <> Text.unpack
            (Text.unwords $ renderInvocationArgs optionStyle command)

    testFilePath :: FilePath
    testFilePath =
      testRootDirectory
        </> commandName
        </> paddedIndex <.> "golden"
      where
        paddedIndex :: FilePath
        paddedIndex = printf "%03d" invocationIndex

    testFileContent :: LBS.LazyByteString
    testFileContent =
      LBS.fromStrict $
        Text.encodeUtf8 $
          Text.unlines $
            renderInvocationWithResult invocation

renderInvocationArgs :: OptionStyle -> Command Raw -> [Text]
renderInvocationArgs style = \case
  Decode DecodeCommand {idText, language} ->
    ["decode", runIdentity idText]
      ++ renderOption style "language" language
  Generate GenerateCommand {count, seed} ->
    ["generate"]
      ++ renderOption style "count" count
      ++ renderOption style "seed" seed
  Validate ValidateCommand {idText} ->
    ["validate", runIdentity idText]

renderInvocationWithResult :: CommandInvocation -> [Text]
renderInvocationWithResult CommandInvocation {command, optionStyle} =
  prompt : output
  where
    args = renderInvocationArgs optionStyle command
    result = evalRand (CLI.run (map Text.unpack args)) (mkStdGen testSeed)
    prompt = "$ taiwan-id " <> Text.unwords args
    output = case result of
      CommandLineSuccess ls -> ls
      CommandLineFailure ls -> ls

renderOption :: Show a => OptionStyle -> Text -> Maybe a -> [Text]
renderOption _ _ Nothing = []
renderOption style name (Just value) = case style of
  EqualSeparated -> ["--" <> name <> "=" <> Text.show value]
  SpaceSeparated -> ["--" <> name, Text.show value]

--------------------------------------------------------------------------------
-- Command invocations
--------------------------------------------------------------------------------

data CommandInvocation = CommandInvocation
  { command :: Command Raw
  , optionStyle :: OptionStyle
  }
  deriving stock (Eq, Show)

data OptionStyle
  = EqualSeparated
  | SpaceSeparated
  deriving stock (Bounded, Enum, Eq, Show)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genCommandDecode :: Gen CommandInvocation
genCommandDecode =
  CommandInvocation
    <$> (Decode <$> genCommand)
    <*> arbitraryBoundedEnum
  where
    genCommand =
      DecodeCommand
        <$> (Identity <$> genIdText)
        <*> arbitrary

genCommandGenerate :: Gen CommandInvocation
genCommandGenerate =
  CommandInvocation
    <$> (Generate <$> genCommand)
    <*> arbitraryBoundedEnum
  where
    genCommand = do
      count <- liftArbitrary (choose (-1, 4))
      seed <- liftArbitrary (choose (1, 1_000_000))
      pure GenerateCommand {count, seed}

genCommandValidate :: Gen CommandInvocation
genCommandValidate =
  CommandInvocation
    <$> (Validate <$> genCommand)
    <*> arbitraryBoundedEnum
  where
    genCommand =
      ValidateCommand
        <$> (Identity <$> genIdText)

genIdText :: Gen Text
genIdText =
  oneof
    [ genIdTextValid
    , genIdTextInvalid
    ]

genIdTextValid :: Gen Text
genIdTextValid = ID.toText . idFromTuple <$> arbitrary

genIdTextInvalid :: Gen Text
genIdTextInvalid =
  oneof
    [ genIdTextInvalidChar
    , genIdTextInvalidChecksum
    , genIdTextInvalidLengthTooLong
    , genIdTextInvalidLengthTooShort
    ]

genIdTextInvalidChar :: Gen Text
genIdTextInvalidChar = do
  idTextValid <- genIdTextValid
  charIndex <- choose (0, 9)
  invalidChar <- elements "+!@#$%^&*()"
  pure $ unsafePokeChar charIndex idTextValid invalidChar

genIdTextInvalidChecksum :: Gen Text
genIdTextInvalidChecksum = do
  idTextValid <- genIdTextValid
  charIndex <- choose (2, 9)
  let charOld = unsafePeekChar charIndex idTextValid
  let charNew = Digit.toChar (unsafeDigitFromChar charOld + 1)
  pure $ unsafePokeChar charIndex idTextValid charNew

genIdTextInvalidLengthTooShort :: Gen Text
genIdTextInvalidLengthTooShort = do
  idTextValid <- genIdTextValid
  invalidLength <- choose (0, 9)
  pure (Text.take invalidLength idTextValid)

genIdTextInvalidLengthTooLong :: Gen Text
genIdTextInvalidLengthTooLong = do
  idTextValid <- genIdTextValid
  extraCharCount <- choose (1, 4)
  extraChars <- Text.pack <$> replicateM extraCharCount genChar
  pure (idTextValid <> extraChars)
  where
    genChar = choose ('0', '9')

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Digit1289 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary ID where
  arbitrary = idFromTuple <$> arbitrary
  shrink = shrinkMap idFromTuple idToTuple

instance Arbitrary Language where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Letter where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

unsafeDigitFromChar :: HasCallStack => Char -> Digit
unsafeDigitFromChar c = case Digit.fromChar c of
  Nothing -> error "unsafeDigitFromChar"
  Just d -> d

unsafePeekChar :: HasCallStack => Int -> Text -> Char
unsafePeekChar i t
  | i < indexMin = outOfBoundsError
  | i > indexMax = outOfBoundsError
  | otherwise = Text.index t i
  where
    indexMin = 0
    indexMax = Text.length t - 1
    outOfBoundsError = error "unsafePeekChar: index out of bounds"

unsafePokeChar :: HasCallStack => Int -> Text -> Char -> Text
unsafePokeChar i t c
  | i < indexMin = outOfBoundsError
  | i > indexMax = outOfBoundsError
  | otherwise = Text.take i t <> Text.singleton c <> Text.drop (i + 1) t
  where
    indexMin = 0
    indexMax = Text.length t - 1
    outOfBoundsError = error "unsafePokeChar: index out of bounds"

idFromTuple :: Digit ~ d => (Letter, Digit1289, d, d, d, d, d, d, d) -> ID
idFromTuple (x0, x1, x2, x3, x4, x5, x6, x7, x8) =
  ID x0 x1 x2 x3 x4 x5 x6 x7 x8

idToTuple :: Digit ~ d => ID -> (Letter, Digit1289, d, d, d, d, d, d, d)
idToTuple (ID x0 x1 x2 x3 x4 x5 x6 x7 x8) =
  (x0, x1, x2, x3, x4, x5, x6, x7, x8)

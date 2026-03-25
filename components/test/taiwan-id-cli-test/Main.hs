{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Functor law" -}

module Main (main) where

import Control.Monad
  ( unless
  )
import Control.Monad.Random
  ( evalRand
  )
import Data.Data
  ( Proxy (Proxy)
  )
import Data.Functor.Identity
  ( Identity (runIdentity)
  )
import Data.Text
  ( Text
  )
import GHC.Stack
  ( HasCallStack
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath
  ( (<.>)
  , (</>)
  )
import System.Random
  ( mkStdGen
  )
import Taiwan.ID.CLI
  ( Command
  , CommandLineResult (CommandLineFailure, CommandLineSuccess)
  , DecodeCommand (DecodeCommand, language)
  , GenerateCommand (GenerateCommand, count, seed)
  , Stage (Raw)
  , ValidateCommand (ValidateCommand, idText)
  )
import Test.QuickCheck
  ( Arbitrary1 (liftArbitrary)
  , Gen
  , arbitraryBoundedEnum
  , choose
  )
import Test.QuickCheck.Gen
  ( unGen
  )
import Test.QuickCheck.Random
  ( mkQCGen
  )
import Test.Tasty
  ( TestTree
  , askOption
  , defaultIngredients
  , defaultMainWithIngredients
  , includingOptions
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  )
import Test.Tasty.Options
  ( IsOption (..)
  , OptionDescription (Option)
  , mkOptionCLParser
  )
import Text.Printf
  ( printf
  )

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Taiwan.ID.CLI as CLI
import qualified Taiwan.ID.CLI as Command
import qualified Taiwan.ID.Test as Test

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

testRootDirectory :: FilePath
testRootDirectory = "data" </> "taiwan-id-cli-test"

-- | Number of golden tests for each command type.
testsPerCommand :: Int
testsPerCommand = 1000

-- | Seed for random number generation when executing CLI commands in tests.
-- Ensures that commands without an explicit '--seed' parameter produce
-- deterministic output.
executionSeed :: Int
executionSeed = 0

-- | Seed for QuickCheck generators that produce test command invocations.
generatorSeed :: Int
generatorSeed = 0

-- | QuickCheck size parameter used by generators.
generatorSize :: Int
generatorSize = 10

--------------------------------------------------------------------------------
-- Modes
--------------------------------------------------------------------------------

-- | The mode in which the test suite runs.
--
-- The default mode (if none is specified) is 'Verify'.
data Mode
  = -- | Create golden files from randomly-generated command invocations.
    -- Existing golden files are never overwritten.
    Create
  | -- | Run each recorded command invocation and overwrite the golden file
    -- with the observed output if it differs from the expected output.
    Update
  | -- | Run each recorded command invocation and fail if the observed output
    -- differs from the expected output recorded in the golden file.
    Verify
  deriving stock (Bounded, Enum, Eq, Show)

instance IsOption Mode where
  defaultValue = Verify
  optionName = pure "mode"
  optionHelp = pure modeHelpText
  optionCLParser = mkOptionCLParser mempty
  parseValue = \case
    "create" -> Just Create
    "update" -> Just Update
    "verify" -> Just Verify
    _ -> Nothing

modeHelpText :: String
modeHelpText =
  "Test mode: create, update, or verify (default)"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

data CommandSpec = CommandSpec
  { commandName :: String
  , commandGen :: Gen (Command Raw)
  }

commandSpecs :: [CommandSpec]
commandSpecs =
  [ CommandSpec "decode" genCommandDecode
  , CommandSpec "generate" genCommandGenerate
  , CommandSpec "validate" genCommandValidate
  ]

main :: IO ()
main = defaultMainWithIngredients ingredients testTree
  where
    ingredients = includingOptions [Option (Proxy @Mode)] : defaultIngredients
    testTree =
      testGroup
        "CLI"
        [ testGroup commandName (testsFromDirectory commandSpec)
        | commandSpec@CommandSpec {commandName} <- commandSpecs
        ]

testsFromDirectory :: CommandSpec -> [TestTree]
testsFromDirectory spec =
  [ testFromIndex spec index
  | index <- take testsPerCommand [0 ..]
  ]

testFromIndex :: CommandSpec -> Int -> TestTree
testFromIndex CommandSpec {commandName, commandGen} index =
  askOption $ \mode ->
    testCase testFilePath $
      case mode of
        Create -> create
        Update -> update
        Verify -> verify
  where
    testDirectoryPath :: FilePath
    testDirectoryPath = testRootDirectory </> commandName

    testFilePath :: FilePath
    testFilePath = testDirectoryPath </> padIndex index <.> "golden"
      where
        padIndex :: Int -> FilePath
        padIndex = printf "%03d"

    create :: IO ()
    create = do
      exists <- doesFileExist testFilePath
      unless exists $ do
        createDirectoryIfMissing True testDirectoryPath
        writeCommandExpectationToFile testFilePath expectation
      where
        expectation :: CommandExpectation
        expectation = commandInvocationToExpectation invocation

        invocation :: CommandInvocation
        invocation =
          unGen
            (genCommandInvocation commandGen)
            (mkQCGen (generatorSeed + index))
            generatorSize

    update :: IO ()
    update = do
      expectation <- readCommandExpectationFromFile testFilePath
      let observedOutputLines =
            runCommandLine (inputCommandLineArgs expectation)
      unless (observedOutputLines == expectedOutputLines expectation) $
        writeCommandExpectationToFile
          testFilePath
          expectation {expectedOutputLines = observedOutputLines}

    verify :: IO ()
    verify = do
      CommandExpectation
        { inputCommandLineArgs
        , expectedOutputLines
        } <-
        readCommandExpectationFromFile testFilePath
      let observedOutputLines = runCommandLine inputCommandLineArgs
      unless (observedOutputLines == expectedOutputLines) $
        assertFailure $
          Text.unpack $
            Text.unlines
              [ "path:"
              , blockIndent [Text.pack testFilePath]
              , "input:"
              , blockIndent [renderPrompt inputCommandLineArgs]
              , "output expected:"
              , blockIndent expectedOutputLines
              , "output observed:"
              , blockIndent observedOutputLines
              ]
      where
        blockIndent :: [Text] -> Text
        blockIndent = Text.unlines . map ("  " <>)

--------------------------------------------------------------------------------
-- Command expectations
--------------------------------------------------------------------------------

-- | Bundles a command with the output we expect from running the command.
data CommandExpectation = CommandExpectation
  { inputCommandLineArgs :: [Text]
  , expectedOutputLines :: [Text]
  }

writeCommandExpectationToFile :: FilePath -> CommandExpectation -> IO ()
writeCommandExpectationToFile
  path
  CommandExpectation {inputCommandLineArgs, expectedOutputLines} =
    TIO.writeFile path $
      Text.unlines $
        renderPrompt inputCommandLineArgs : expectedOutputLines

readCommandExpectationFromFile
  :: HasCallStack => FilePath -> IO CommandExpectation
readCommandExpectationFromFile path = do
  contents <- TIO.readFile path
  case Text.lines contents of
    [] -> reportEmptyFile
    (prompt : expectedOutputLines) ->
      case parsePrompt prompt of
        Just inputCommandLineArgs ->
          pure CommandExpectation {inputCommandLineArgs, expectedOutputLines}
        Nothing ->
          reportInvalidPrompt
  where
    reportEmptyFile =
      assertFailure $
        unwords
          [ "readCommandExpectationFromFile: empty file:"
          , path
          ]
    reportInvalidPrompt =
      assertFailure $
        unwords
          [ "readCommandExpectationFromFile: invalid prompt line in:"
          , path
          ]

-- | The prefix used for prompt lines in golden files.
promptPrefix :: Text
promptPrefix = "$ taiwan-id "

-- | Parse the arguments from a prompt line of the form:
-- @$ taiwan-id <args...>@
-- Returns 'Nothing' if the line does not have the expected prefix.
parsePrompt :: Text -> Maybe [Text]
parsePrompt = fmap Text.words . Text.stripPrefix promptPrefix

-- | Render a list of arguments as a prompt line of the form:
-- @$ taiwan-id <args...>@
renderPrompt :: [Text] -> Text
renderPrompt args = promptPrefix <> Text.intercalate " " args

--------------------------------------------------------------------------------
-- CLI execution
--------------------------------------------------------------------------------

runCommandLine :: [Text] -> [Text]
runCommandLine args =
  case result of
    CommandLineSuccess ls -> ls
    CommandLineFailure ls -> ls
  where
    result =
      evalRand
        (CLI.run (map Text.unpack args))
        (mkStdGen executionSeed)

commandInvocationToExpectation :: CommandInvocation -> CommandExpectation
commandInvocationToExpectation CommandInvocation {command, optionStyle} =
  CommandExpectation
    { inputCommandLineArgs
    , expectedOutputLines = runCommandLine inputCommandLineArgs
    }
  where
    inputCommandLineArgs = renderInvocationArgs optionStyle command

renderInvocationArgs :: OptionStyle -> Command Raw -> [Text]
renderInvocationArgs style = \case
  Command.Decode DecodeCommand {idText, language} ->
    ["decode", runIdentity idText]
      ++ renderOption style "language" language
  Command.Generate GenerateCommand {count, seed} ->
    ["generate"]
      ++ renderOption style "count" count
      ++ renderOption style "seed" seed
  Command.Validate ValidateCommand {idText} ->
    ["validate", runIdentity idText]

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

genCommandInvocation :: Gen (Command Raw) -> Gen CommandInvocation
genCommandInvocation genCommand =
  CommandInvocation
    <$> genCommand
    <*> genOptionStyle

genCommandDecode :: Gen (Command Raw)
genCommandDecode = do
  idText <- liftArbitrary Test.genIDText
  language <- liftArbitrary Test.genLanguage
  pure $ Command.Decode $ DecodeCommand {idText, language}

genCommandGenerate :: Gen (Command Raw)
genCommandGenerate = do
  count <- liftArbitrary (choose (-1, 4))
  seed <- liftArbitrary (choose (1, 1_000_000))
  pure $ Command.Generate $ GenerateCommand {count, seed}

genCommandValidate :: Gen (Command Raw)
genCommandValidate = do
  idText <- liftArbitrary Test.genIDText
  pure $ Command.Validate $ ValidateCommand {idText}

genOptionStyle :: Gen OptionStyle
genOptionStyle = arbitraryBoundedEnum

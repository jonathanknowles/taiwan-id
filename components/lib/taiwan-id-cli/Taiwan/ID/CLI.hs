{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use newtype instead of data" -}

module Taiwan.ID.CLI where

import Control.Monad
  ( replicateM
  )
import Control.Monad.Random
  ( MonadRandom
  , evalRand
  , getRandom
  )
import Data.Functor.Identity
  ( Identity (Identity)
  , runIdentity
  )
import Data.Kind
  ( Type
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Options.Applicative
  ( CommandFields
  , Mod
  , Parser
  , ParserInfo
  , ParserResult (CompletionInvoked, Failure, Success)
  , argument
  , auto
  , command
  , eitherReader
  , execParserPure
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , long
  , metavar
  , option
  , optional
  , prefs
  , progDesc
  , renderFailure
  , showHelpOnEmpty
  , str
  , (<**>)
  )
import System.Random
  ( mkStdGen
  )
import Taiwan.ID
  ( ID
  )
import Taiwan.ID.CharIndex
  ( CharIndex (CharIndex)
  )
import Taiwan.ID.CharSet
  ( CharSet (CharRange, CharSet)
  )
import Taiwan.ID.Language
  ( Language (Chinese, English)
  )

import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Taiwan.ID as ID
import qualified Taiwan.ID.Gender as Gender
import qualified Taiwan.ID.Issuer as Issuer
import qualified Taiwan.ID.Region as Region

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

data Stage = Raw | Resolved

type family Optional (s :: Stage) :: Type -> Type where
  Optional Raw = Maybe
  Optional Resolved = Identity

type family Required (s :: Stage) :: Type -> Type where
  Required Raw = Identity
  Required Resolved = Identity

data Command (s :: Stage)
  = Decode (DecodeCommand s)
  | Generate (GenerateCommand s)
  | Validate (ValidateCommand s)

deriving instance Eq (Command Raw)
deriving instance Show (Command Raw)

type CommandDescription = Mod CommandFields (Command Raw)
type CommandParser args = Parser (args Raw)
type CommandResolver m args = args Raw -> m (args Resolved)
type CommandRunner args = args Resolved -> CommandLineResult

--------------------------------------------------------------------------------
-- Results
--------------------------------------------------------------------------------

data CommandLineResult
  = -- | Lines to be printed to stdout, with exit code 0.
    CommandLineSuccess [Text]
  | -- | Lines to be printed to stderr, with exit code 1.
    CommandLineFailure [Text]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

run :: MonadRandom m => [String] -> m CommandLineResult
run args =
  case execParserPure (prefs showHelpOnEmpty) topLevelParser args of
    Failure failure ->
      pure $
        CommandLineFailure
          (T.lines $ T.pack $ fst $ renderFailure failure "taiwan-id")
    CompletionInvoked _ ->
      pure $ CommandLineSuccess []
    Success unresolvedCommand -> do
      resolved <- commandResolver unresolvedCommand
      pure $ case resolved of
        Decode resolvedCommand -> decodeCommandRunner resolvedCommand
        Generate resolvedCommand -> generateCommandRunner resolvedCommand
        Validate resolvedCommand -> validateCommandRunner resolvedCommand

topLevelParser :: ParserInfo (Command Raw)
topLevelParser =
  info
    (commandParser <**> helper)
    $ mconcat
      [ fullDesc
      , progDesc "Tools for working with Taiwan uniform identification numbers"
      , header "taiwan-id - Taiwan uniform identification number tools"
      ]

commandParser :: CommandParser Command
commandParser =
  hsubparser $
    mconcat
      [ generateCommandDescription
      , validateCommandDescription
      , decodeCommandDescription
      ]

commandResolver :: MonadRandom m => CommandResolver m Command
commandResolver = \case
  Decode args -> Decode <$> decodeCommandResolver args
  Generate args -> Generate <$> generateCommandResolver args
  Validate args -> Validate <$> validateCommandResolver args

--------------------------------------------------------------------------------
-- Decode
--------------------------------------------------------------------------------

decodeCommandDescription :: CommandDescription
decodeCommandDescription =
  command
    "decode"
    ( info
        (Decode <$> decodeCommandParser)
        (progDesc "Decode an identification number")
    )

data DecodeCommand (s :: Stage) = DecodeCommand
  { idText :: Required s Text
  , language :: Optional s Language
  }

deriving instance Eq (DecodeCommand Raw)
deriving instance Show (DecodeCommand Raw)

decodeCommandParser :: CommandParser DecodeCommand
decodeCommandParser =
  DecodeCommand
    <$> argument str (metavar "ID")
    <*> languageOption
  where
    languageOption :: Parser (Maybe Language)
    languageOption =
      optional $
        option
          (eitherReader parseLanguage)
          ( long "language"
              <> metavar "LANG"
              <> help "Output language: English or Chinese (default: English)"
          )
    parseLanguage :: String -> Either String Language
    parseLanguage = \case
      "English" -> Right English
      "Chinese" -> Right Chinese
      s ->
        Left $
          "Unknown language '"
            <> s
            <> "': expected 'English' or 'Chinese'"

decodeCommandResolver :: Applicative m => CommandResolver m DecodeCommand
decodeCommandResolver DecodeCommand {idText, language} =
  pure
    DecodeCommand
      { idText
      , language = Identity (fromMaybe defaultLanguage language)
      }
  where
    defaultLanguage :: Language
    defaultLanguage = English

decodeCommandRunner :: CommandRunner DecodeCommand
decodeCommandRunner DecodeCommand {idText, language} =
  case ID.fromText (resolve idText) of
    Left err ->
      CommandLineFailure $
        renderIdFromTextError (resolve idText) err
    Right i ->
      CommandLineSuccess $
        T.lines $
          formatFields (resolve language) i

formatFields :: Language -> ID -> Text
formatFields language i =
  T.unlines (map (formatField maxWidth language) fields)
  where
    maxWidth = maximum (map (T.length . fst) fields)
    fields =
      [ (issuerKey language, Issuer.toText language (ID.getIssuer i))
      , (genderKey language, Gender.toText language (ID.getGender i))
      , (regionKey language, Region.toText language (ID.getRegion i))
      ]
    issuerKey = \case
      English -> "Issuer"
      Chinese -> "核發機關"
    genderKey = \case
      English -> "Gender"
      Chinese -> "性別"
    regionKey = \case
      English -> "Region"
      Chinese -> "地區"

formatField :: Int -> Language -> (Text, Text) -> Text
formatField maxWidth language (k, v) =
  T.concat
    [ k
    , padding language
    , separator language
    , v
    ]
  where
    padding =
      T.replicate (maxWidth - T.length k) . \case
        English -> " "
        Chinese -> "\x3000"
    separator = \case
      English -> ": "
      Chinese -> "："

--------------------------------------------------------------------------------
-- Generate
--------------------------------------------------------------------------------

generateCommandDescription :: CommandDescription
generateCommandDescription =
  command
    "generate"
    ( info
        (Generate <$> generateCommandParser)
        (progDesc "Generate one or more random identification numbers")
    )

data GenerateCommand (s :: Stage) = GenerateCommand
  { count :: Optional s Int
  , seed :: Optional s Int
  }

deriving instance Eq (GenerateCommand Raw)
deriving instance Show (GenerateCommand Raw)

generateCommandParser :: CommandParser GenerateCommand
generateCommandParser =
  GenerateCommand
    <$> optional
      ( option
          auto
          ( long "count"
              <> metavar "N"
              <> help
                "Number of identification numbers to generate (default: 1)"
          )
      )
    <*> optional
      ( option
          auto
          ( long "seed"
              <> metavar "N"
              <> help "Random seed for reproducible generation"
          )
      )

generateCommandResolver :: MonadRandom m => CommandResolver m GenerateCommand
generateCommandResolver GenerateCommand {count, seed} = do
  resolvedSeed <- maybe getRandom pure seed
  pure
    GenerateCommand
      { count = Identity (fromMaybe defaultCount count)
      , seed = Identity resolvedSeed
      }
  where
    defaultCount :: Int
    defaultCount = 1

generateCommandRunner :: CommandRunner GenerateCommand
generateCommandRunner GenerateCommand {count, seed}
  | count < 1 =
      CommandLineFailure
        ["Count argument should be a non-zero natural number."]
  | otherwise =
      CommandLineSuccess $
        map ID.toText $
          evalRand
            (replicateM (resolve count) ID.generate)
            (mkStdGen (resolve seed))

--------------------------------------------------------------------------------
-- Validate
--------------------------------------------------------------------------------

validateCommandDescription :: CommandDescription
validateCommandDescription =
  command
    "validate"
    ( info
        (Validate <$> validateCommandParser)
        (progDesc "Validate an identification number")
    )

data ValidateCommand (s :: Stage) = ValidateCommand
  { idText :: Required s Text
  }

deriving instance Eq (ValidateCommand Raw)
deriving instance Show (ValidateCommand Raw)

validateCommandParser :: CommandParser ValidateCommand
validateCommandParser = ValidateCommand <$> argument str (metavar "ID")

validateCommandResolver :: Applicative m => CommandResolver m ValidateCommand
validateCommandResolver ValidateCommand {idText} = pure ValidateCommand {idText}

validateCommandRunner :: CommandRunner ValidateCommand
validateCommandRunner ValidateCommand {idText} =
  case ID.fromText (resolve idText) of
    Right _ ->
      CommandLineSuccess []
    Left err ->
      CommandLineFailure $
        renderIdFromTextError (resolve idText) err

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

renderIdFromTextError :: Text -> ID.FromTextError -> [Text]
renderIdFromTextError input =
  \case
    ID.InvalidChecksum ->
      [ "Invalid checksum."
      ]
    ID.InvalidLength ->
      [ "Invalid length."
      , "An identification number must be exactly 10 characters in length."
      ]
    ID.InvalidChar (CharIndex i) charSet ->
      [ "Invalid character:"
      , input
      , Text.replicate i (Text.singleton ' ') <> "^"
      , "Character at this position must be " <> describeCharSet charSet <> "."
      ]

describeCharSet :: CharSet -> Text
describeCharSet = \case
  CharRange lo hi ->
    "a character in the range "
      <> "["
      <> Text.singleton lo
      <> " .. "
      <> Text.singleton hi
      <> "]"
  CharSet neSet ->
    "a character from the set "
      <> "{"
      <> Text.intercalate ", " (Text.singleton <$> Foldable.toList neSet)
      <> "}"

resolve :: Identity a -> a
resolve = runIdentity

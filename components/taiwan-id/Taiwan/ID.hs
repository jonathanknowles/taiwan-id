{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Taiwan.ID
  (
  -- * Type
    ID (..)

  -- * Construction
  , fromSymbol
  , fromText
  , FromTextError (..)

  -- * Conversion
  , toText

  -- * Validity
  , checksum

  -- * Generation
  , generate

  -- * Inspection
  , getGender
  , getLocation
  , getNationality

  -- * Modification
  , setGender
  , setLocation
  , setNationality
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom )
import Data.Bifunctor
  ( Bifunctor (first) )
import Data.Finitary
  ( Finitary )
import Data.Proxy
  ( Proxy (Proxy) )
import Data.Text
  ( Text )
import GHC.Generics
  ( Generic )
import GHC.TypeLits
  ( Symbol, symbolVal )
import Taiwan.ID.CharIndex
  ( CharIndex (..) )
import Taiwan.ID.CharSet
  ( CharSet (..) )
import Taiwan.ID.Digit
  ( Digit (..) )
import Taiwan.ID.Digit1289
  ( Digit1289 (..) )
import Taiwan.ID.Gender
  ( Gender (..) )
import Taiwan.ID.Letter
  ( Letter (..) )
import Taiwan.ID.Location
  ( Location )
import Taiwan.ID.Nationality
  ( Nationality (..) )
import Taiwan.ID.Unchecked
  ( UncheckedID (UncheckedID), ValidID )
import Taiwan.ID.Utilities
  ( guard, randomFinitary )
import Text.Read
  ( Lexeme (Ident, Symbol, Punc), Read (readPrec), lexP, parens, prec )

import qualified Data.Text as T
import qualified Taiwan.ID.Location as Location
import qualified Taiwan.ID.Unchecked as U

-- |
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Taiwan.ID
-- >>> import qualified Taiwan.ID as ID

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Represents a __valid__ 10-digit uniform identification number of the form
-- __@A123456789@__.
--
-- By construction, invalid identification numbers are __not representable__ by
-- this data type.
--
-- To guarantee correctness, an 'ID' value does __not__ store the terminal
-- checksum digit of the identification number it represents.
--
-- To compute the checksum digit, use 'checksum'.
--
data ID = ID
  { c0 :: !Letter
  , c1 :: !Digit1289
  , c2 :: !Digit
  , c3 :: !Digit
  , c4 :: !Digit
  , c5 :: !Digit
  , c6 :: !Digit
  , c7 :: !Digit
  , c8 :: !Digit
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass Finitary

instance Read ID where
  readPrec = parens $ prec 10 $ do
    Ident  "ID"         <- lexP
    Symbol "."          <- lexP
    Ident  "fromSymbol" <- lexP
    Punc   "@"          <- lexP
    unsafeFromText <$> readPrec

instance Show ID where
  showsPrec d s =
    showParen (d > 10) $
      showString "ID.fromSymbol @" . shows (toText s)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs an 'ID' from a type-level textual symbol.
--
-- The symbol must be exactly 10 characters in length and of the form
-- __@A123456789@__:
--
-- >>> ID.fromSymbol @"A123456789"
-- ID.fromSymbol @"A123456789"
--
-- More precisely:
--
--  - the symbol must match the regular expression __@^[A-Z][1289][0-9]{8}$@__
--  - the resultant ID must have a valid checksum.
--
-- Failure to satisfy these constraints will result in one of the following
-- __type errors__:
--
-- === Invalid lengths
--
-- >>> ID.fromSymbol @"A12345678"
-- ...
-- ... An ID must have exactly 10 characters.
-- ...
--
-- === Invalid checksums
--
-- >>> ID.fromSymbol @"A123456780"
-- ...
-- ... ID has invalid checksum.
-- ...
--
-- === Invalid characters
--
-- On detection of an invalid character, the resulting type error reports
-- both the position of the character and the set of characters permitted
-- at that position.
--
-- >>> ID.fromSymbol @"_123456789"
-- ...
--     • "_123456789"
--        ^
--       Character at this position must be an uppercase letter.
-- ...
--
-- >>> ID.fromSymbol @"A_23456789"
-- ...
--     • "A_23456789"
--         ^
--       Character at this position must be a digit from the set {1, 2, 8, 9}.
-- ...
--
-- >>> ID.fromSymbol @"A1_3456789"
-- ...
--     • "A1_3456789"
--          ^
--       Character at this position must be a digit in the range [0 .. 9].
-- ...
--
-- === Missing or empty symbols
--
-- >>> ID.fromSymbol
-- ...
-- ... Expected a type-level symbol of the form "A123456789".
-- ...
--
-- >>> ID.fromSymbol @""
-- ...
-- ... Expected a type-level symbol of the form "A123456789".
-- ...
--
fromSymbol :: forall (s :: Symbol). ValidID s => ID
fromSymbol = unsafeFromText $ T.pack $ symbolVal $ Proxy @s

-- | Attempts to construct an 'ID' from 'Text'.
--
-- The input must be exactly 10 characters in length and of the form
-- __@A123456789@__:
--
-- >>> ID.fromText "A123456789"
-- Right (ID.fromSymbol @"A123456789")
--
-- More precisely, the input must match the regular expression
-- __@^[A-Z][1289][0-9]{8}$@__, and the resultant ID must have a valid
-- checksum.
--
-- This function satisfies the following law:
--
-- @
-- 'fromText' ('toText' i) '==' 'Right' i
-- @
--
fromText :: Text -> Either FromTextError ID
fromText text = do
    unchecked <- first fromUncheckedError $ U.fromText text
    guard InvalidChecksum $ fromUnchecked unchecked
  where
    fromUncheckedError :: U.FromTextError -> FromTextError
    fromUncheckedError = \case
      U.InvalidChar i r ->
        InvalidChar i r
      U.InvalidLength ->
        InvalidLength

unsafeFromText :: Text -> ID
unsafeFromText t =
  case fromText t of
    Left _ -> error "unsafeFromText"
    Right i -> i

-- | Indicates an error that occurred while constructing an 'ID' from 'Text'.
--
data FromTextError

  = InvalidChar CharIndex CharSet
  -- ^ Indicates that the input text contains a character that is not allowed.
  --
  --   - `CharIndex` specifies the position of the invalid character.
  --   - `CharSet` specifies the set of characters allowed at that position.

  | InvalidChecksum
  -- ^ Indicates that the parsed identification number has an invalid checksum.

  | InvalidLength
  -- ^ Indicates that the input text has an invalid length.

  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | Converts an 'ID' to 'Text'.
--
-- The output is of the form __@A123456789@__.
--
-- This function satisfies the following law:
--
-- @
-- 'fromText' ('toText' i) '==' 'Right' i
-- @
--
toText :: ID -> Text
toText = U.toText . toUnchecked

--------------------------------------------------------------------------------
-- Validity
--------------------------------------------------------------------------------

-- | Computes the terminal checksum digit of an 'ID'.
--
checksum :: ID -> Digit
checksum (ID u0 u1 u2 u3 u4 u5 u6 u7 u8) =
  negate $ U.checksum (UncheckedID u0 u1 u2 u3 u4 u5 u6 u7 u8 0)

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Generates a random 'ID'.
--
generate :: MonadRandom m => m ID
generate = randomFinitary

--------------------------------------------------------------------------------
-- Inspection
--------------------------------------------------------------------------------

-- | Decodes the 'Gender' component of an 'ID'.
--
getGender :: ID -> Gender
getGender ID {c1} = fst $ decodeC1 c1

-- | Decodes the 'Location' component of an 'ID'.
--
getLocation :: ID -> Location
getLocation ID {c0} = Location.fromLetter c0

-- | Decodes the 'Nationality' component of an 'ID'.
--
getNationality :: ID -> Nationality
getNationality ID {c1} = snd $ decodeC1 c1

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Updates the 'Gender' component of an 'ID'.
--
setGender :: Gender -> ID -> ID
setGender gender i = i {c1 = encodeC1 (gender, getNationality i)}

-- | Updates the 'Location' component of an 'ID'.
--
setLocation :: Location -> ID -> ID
setLocation location i = i {c0 = Location.toLetter location}

-- | Updates the 'Nationality' component of an 'ID'.
--
setNationality :: Nationality -> ID -> ID
setNationality nationality i = i {c1 = encodeC1 (getGender i, nationality)}

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

decodeC1 :: Digit1289 -> (Gender, Nationality)
decodeC1 = \case
  D1289_1 -> (  Male,    National)
  D1289_2 -> (Female,    National)
  D1289_8 -> (  Male, NonNational)
  D1289_9 -> (Female, NonNational)

encodeC1 :: (Gender, Nationality) -> Digit1289
encodeC1 = \case
  (  Male,    National) -> D1289_1
  (Female,    National) -> D1289_2
  (  Male, NonNational) -> D1289_8
  (Female, NonNational) -> D1289_9

fromUnchecked :: UncheckedID -> Maybe ID
fromUnchecked u@(UncheckedID u0 u1 u2 u3 u4 u5 u6 u7 u8 _) =
    case U.checksumValidity u of
      U.ChecksumValid   -> Just i
      U.ChecksumInvalid -> Nothing
  where
    i = ID u0 u1 u2 u3 u4 u5 u6 u7 u8

toUnchecked :: ID -> UncheckedID
toUnchecked i@(ID u0 u1 u2 u3 u4 u5 u6 u7 u8) =
  UncheckedID u0 u1 u2 u3 u4 u5 u6 u7 u8 (checksum i)

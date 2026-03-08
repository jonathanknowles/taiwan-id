{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Digit1289
  ( Digit1289 (..)
  , fromChar
  , toChar
  , toDigit
  , generate
  , FromChar
  , FromNat
  , ToNat
  ) where

import Control.Monad.Random
  ( MonadRandom )
import Data.Finitary
  ( Finitary )
import GHC.Generics
  ( Generic )
import GHC.TypeNats
  ( Nat )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Utilities
  ( randomFinitary )

-- | Represents a single decimal digit from the set {@1@, @2@, @8@, @9@}.
--
data Digit1289
  = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass Finitary

-- | Attempts to parse a 'Digit1289' from a character.
--
-- Only the characters @'1'@, @'2'@, @'8'@, and @'9'@ are accepted.
--
fromChar :: Char -> Maybe Digit1289
fromChar = \case
  '1' -> Just D1289_1
  '2' -> Just D1289_2
  '8' -> Just D1289_8
  '9' -> Just D1289_9
  _   -> Nothing

-- | Converts a 'Digit1289' to a decimal digit character.
--
toChar :: Digit1289 -> Char
toChar = \case
  D1289_1 -> '1'
  D1289_2 -> '2'
  D1289_8 -> '8'
  D1289_9 -> '9'

-- | Converts a 'Digit1289' to an ordinary 'Digit'.
--
toDigit :: Digit1289 -> Digit
toDigit = \case
  D1289_1 -> D1
  D1289_2 -> D2
  D1289_8 -> D8
  D1289_9 -> D9

-- | Generates a random 'Digit1289'.
--
generate :: MonadRandom m => m Digit1289
generate = randomFinitary

type family FromChar (c :: Char) :: Maybe Digit1289 where
  FromChar '1' = Just D1289_1
  FromChar '2' = Just D1289_2
  FromChar '8' = Just D1289_8
  FromChar '9' = Just D1289_9
  FromChar _   = Nothing

type family FromNat (n :: Nat) :: Maybe Digit1289 where
  FromNat 1 = Just D1289_1
  FromNat 2 = Just D1289_2
  FromNat 8 = Just D1289_8
  FromNat 9 = Just D1289_9
  FromNat _ = Nothing

type family ToNat (d :: Digit1289) :: Nat where
  ToNat D1289_1 = 1
  ToNat D1289_2 = 2
  ToNat D1289_8 = 8
  ToNat D1289_9 = 9

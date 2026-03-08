{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Digit
  ( Digit (..)
  , fromChar
  , toChar
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
import ROC.ID.Utilities
  ( maybeFinitary, randomFinitary )
import Text.Read
  ( Read (readPrec), readMaybe )

import Prelude hiding
  ( fromIntegral )

import qualified Prelude

-- | Represents a single decimal digit in the range @0@ to @9@.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving stock (Bounded, Enum, Eq, Generic, Ord)
  deriving anyclass Finitary

-- | Arithmetic modulo 10.
--
instance Num Digit where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum D0 = D0
  signum __ = D1

instance Read Digit where readPrec = fromInteger <$> readPrec

instance Show Digit where show = show . fromEnum

-- | Attempts to parse a 'Digit' from a character.
--
-- The 'Char' must be a decimal digit in the range @0@ to @9@.
--
fromChar :: Char -> Maybe Digit
fromChar c = readMaybe [c] >>= maybeFinitary

-- | Converts a 'Digit' to a decimal digit character.
--
toChar :: Digit -> Char
toChar digit = case show digit of
  [c] -> c
  _ -> error "toChar"

-- | Creates a 'Digit' from an integral number (modulo 10).
--
fromIntegral :: Integral i => i -> Digit
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 10))

-- | Generates a random digit.
--
generate :: MonadRandom m => m Digit
generate = randomFinitary

type family FromChar (c :: Char) :: Maybe Digit where
  FromChar '0' = Just D0
  FromChar '1' = Just D1
  FromChar '2' = Just D2
  FromChar '3' = Just D3
  FromChar '4' = Just D4
  FromChar '5' = Just D5
  FromChar '6' = Just D6
  FromChar '7' = Just D7
  FromChar '8' = Just D8
  FromChar '9' = Just D9
  FromChar _   = Nothing

type family FromNat (n :: Nat) :: Maybe Digit where
  FromNat 0 = Just D0
  FromNat 1 = Just D1
  FromNat 2 = Just D2
  FromNat 3 = Just D3
  FromNat 4 = Just D4
  FromNat 5 = Just D5
  FromNat 6 = Just D6
  FromNat 7 = Just D7
  FromNat 8 = Just D8
  FromNat 9 = Just D9
  FromNat _ = Nothing

type family ToNat (d :: Digit) :: Nat where
  ToNat D0 = 0
  ToNat D1 = 1
  ToNat D2 = 2
  ToNat D3 = 3
  ToNat D4 = 4
  ToNat D5 = 5
  ToNat D6 = 6
  ToNat D7 = 7
  ToNat D8 = 8
  ToNat D9 = 9

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Letter
  ( Letter (..)
  , fromChar
  , toChar
  , generate
  , FromChar
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary )
import GHC.Generics
  ( Generic )
import ROC.ID.Utilities
  ( randomFinitary )
import Text.Read
  ( readMaybe )

-- | Represents a letter in the latin alphabet.
--
data Letter
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass Finitary

-- | Attempts to parse a 'Letter' from a character.
--
-- Returns 'Nothing' if the specified character is not an uppercase alphabetic
-- character.
--
fromChar :: Char -> Maybe Letter
fromChar c = readMaybe [c]

-- | Converts the specified 'Letter' to a character.
--
toChar :: Letter -> Char
toChar letter = case show letter of
  [c] -> c
  _ -> error "toChar"

-- | Generates a random 'Letter'.
--
generate :: MonadRandom m => m Letter
generate = randomFinitary

type family FromChar (c :: Char) :: Maybe Letter where
  FromChar 'A' = Just A; FromChar 'N' = Just N
  FromChar 'B' = Just B; FromChar 'O' = Just O
  FromChar 'C' = Just C; FromChar 'P' = Just P
  FromChar 'D' = Just D; FromChar 'Q' = Just Q
  FromChar 'E' = Just E; FromChar 'R' = Just R
  FromChar 'F' = Just F; FromChar 'S' = Just S
  FromChar 'G' = Just G; FromChar 'T' = Just T
  FromChar 'H' = Just H; FromChar 'U' = Just U
  FromChar 'I' = Just I; FromChar 'V' = Just V
  FromChar 'J' = Just J; FromChar 'W' = Just W
  FromChar 'K' = Just K; FromChar 'X' = Just X
  FromChar 'L' = Just L; FromChar 'Y' = Just Y
  FromChar 'M' = Just M; FromChar 'Z' = Just Z
  FromChar _ = Nothing

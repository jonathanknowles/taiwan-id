{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ROC.ID.CharIndex
  ( CharIndex (..)
  )
  where

-- | Specifies the zero-based position of a character within a string.
--
newtype CharIndex = CharIndex Int
  deriving stock (Read, Show)
  deriving newtype (Enum, Eq, Num, Ord)

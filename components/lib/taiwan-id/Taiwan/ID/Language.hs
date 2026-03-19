{-# LANGUAGE DerivingStrategies #-}

module Taiwan.ID.Language where

-- | A language into which values can be localized when pretty printing.
data Language = English | Chinese
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

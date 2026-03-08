{-# LANGUAGE DerivingStrategies #-}

module ROC.ID.CharSet
  ( CharSet (..)
  )
  where

import Data.Set.NonEmpty
  ( NESet )

-- | Specifies a set of characters.
--
data CharSet
  = CharSet (NESet Char)
  -- ^ An explicit set of characters.
  | CharRange Char Char
  -- ^ An inclusive range of characters.
  deriving stock (Eq, Ord, Read, Show)

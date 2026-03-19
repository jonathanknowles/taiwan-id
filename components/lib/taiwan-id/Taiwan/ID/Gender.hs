{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Taiwan.ID.Gender
  ( Gender (..)
  , toText
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary )
import Data.Text
  ( Text )
import GHC.Generics
  ( Generic )
import Taiwan.ID.Language
  ( Language (..) )
import Taiwan.ID.Utilities
  ( randomFinitary )

-- | A person's gender.
--
data Gender = Male | Female
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass Finitary

-- | Prints the specified 'Gender'.
--
toText :: Language -> Gender -> Text
toText = \case
  English -> toTextEnglish
  Chinese -> toTextChinese

toTextEnglish :: Gender -> Text
toTextEnglish = \case
  Male   -> "Male"
  Female -> "Female"

toTextChinese :: Gender -> Text
toTextChinese = \case
  Male   -> "男性"
  Female -> "女性"

-- | Generates a random 'Gender'.
--
generate :: MonadRandom m => m Gender
generate = randomFinitary

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Taiwan.ID.Issuer
  ( Issuer (..)
  , toText
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary )
import Data.Text
  ( Text )
import GHC.Generics
  ( Generic )
import Taiwan.ID.Language
  ( Language (English, Chinese) )
import Taiwan.ID.Utilities
  ( randomFinitary )

-- | A government authority that issues identification numbers.
--
data Issuer
  = HouseholdRegistrationOffice
  | NationalImmigrationAgency
  deriving stock (Bounded, Enum, Eq, Ord, Generic, Read, Show)
  deriving anyclass Finitary

-- | Generates a random 'Issuer'.
--
generate :: MonadRandom m => m Issuer
generate = randomFinitary

-- | Prints the specified 'Issuer'.
--
toText :: Language -> Issuer -> Text
toText = \case
  English -> toTextEnglish
  Chinese -> toTextChinese

toTextChinese :: Issuer -> Text
toTextChinese = \case
  HouseholdRegistrationOffice ->
    "戶政事務所"
  NationalImmigrationAgency ->
    "移民署"

toTextEnglish :: Issuer -> Text
toTextEnglish = \case
  HouseholdRegistrationOffice ->
    "Household Registration Office"
  NationalImmigrationAgency ->
    "National Immigration Agency"

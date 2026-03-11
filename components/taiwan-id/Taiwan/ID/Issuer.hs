{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Taiwan.ID.Issuer
  ( Issuer (..)
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary )
import GHC.Generics
  ( Generic )
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

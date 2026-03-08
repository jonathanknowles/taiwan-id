{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module ROC.ID.Nationality
  ( Nationality (..)
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary )
import GHC.Generics
  ( Generic )
import ROC.ID.Utilities
  ( randomFinitary )

-- | Specifies a person's nationality.
--
data Nationality = National | NonNational
  deriving stock (Bounded, Enum, Eq, Ord, Generic, Read, Show)
  deriving anyclass Finitary

-- | Generates a random 'Nationality'.
--
generate :: MonadRandom m => m Nationality
generate = randomFinitary

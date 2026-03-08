{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Utilities where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Finitary
  ( Finitary (fromFinite), Cardinality )
import Data.Finite
  ( packFinite )
import Data.Maybe
  ( fromMaybe )
import Data.Proxy
  ( Proxy (Proxy) )
import GHC.TypeLits
  ( Symbol, UnconsSymbol, ConsSymbol, type (<=), type (+) )
import GHC.TypeNats
  ( Nat, type (-), natVal )
import Numeric.Natural
  ( Natural )

cardinality :: forall a. Finitary a => Natural
cardinality = natVal $ Proxy @(Cardinality a)

guard :: x -> Maybe y -> Either x y
guard x = maybe (Left x) Right

maybeFinitary :: forall a. Finitary a => Natural -> Maybe a
maybeFinitary = fmap fromFinite . packFinite . fromIntegral @Natural @Integer

randomFinitary
  :: forall m a. (MonadRandom m, Finitary a, 1 <= Cardinality a) => m a
randomFinitary =
    fromMaybe failure . maybeFinitary <$> randomNatural (0, cardinality @a - 1)
  where
    failure = error "randomFinitary: unexpected out-of-bounds value"

randomNatural :: MonadRandom m => (Natural, Natural) -> m Natural
randomNatural (lo, hi) =
  fromIntegral @Integer @Natural <$>
    getRandomR
      ( fromIntegral @Natural @Integer lo
      , fromIntegral @Natural @Integer hi
      )

type family FromJust (m :: Maybe a) e where
  FromJust Nothing e = e
  FromJust (Just a) _ = a

type family Fst (t :: (a, a)) :: a where
  Fst '(x, _) = x

type family Snd (t :: (a, a)) :: a where
  Snd '(_, y) = y

type family ListLength (as :: [a]) :: Nat where
  ListLength as = ListLengthInner 0 as

type family ListLengthInner (n :: Nat) (as :: [a]) :: Nat where
  ListLengthInner n '[] = n
  ListLengthInner n (a : as) = ListLengthInner (n + 1) as

type family SymbolLength (s :: Symbol) :: Nat where
  SymbolLength s = ListLength (SymbolToCharList s)

type family
    SymbolToCharList (s :: Symbol) :: [Char]
  where
    SymbolToCharList s = MaybeCharSymbolToCharList (UnconsSymbol s)

type family
  MaybeCharSymbolToCharList
    (m :: Maybe (Char, Symbol)) :: [Char]
  where
    MaybeCharSymbolToCharList ('Just '(c, rest)) = c ': SymbolToCharList rest
    MaybeCharSymbolToCharList 'Nothing = '[]

type family ReplicateChar (n :: Nat) (c :: Char) :: Symbol where
  ReplicateChar n c = ReplicateChar' "" n c

type family ReplicateChar' (s :: Symbol) (n :: Nat) (c :: Char) :: Symbol where
  ReplicateChar' s 0 _ = s
  ReplicateChar' s n c = ReplicateChar' (ConsSymbol c s) (n - 1) c

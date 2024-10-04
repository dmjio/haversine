{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Geo.Haversine.Types
  ( -- * Types
    Coord (..)
  , Bearing (..)
  , Radius (..)
  , Distance (..)
  , DistanceType (..)
  -- * Classes
  , HasRadius (..)
  -- * Constants
  , earthRadiusMiles
  , earthRadiusKms
  ) where

import Data.AEq         ((~==), AEq)
import Test.QuickCheck

data Coord = Coord { getLat, getLng :: Double }
  deriving stock (Show, Eq)

earthRadiusMiles :: Double
earthRadiusMiles = 3963.1906

earthRadiusKms :: Double
earthRadiusKms = 6378.137

instance Arbitrary Coord where
  arbitrary = do
    lat <- choose (-90, 90)
    lng <- choose (-180, 180)
    pure (Coord lat lng)

instance AEq Coord where
  Coord la1 ln1 ~== Coord la2 ln2 =
    la1 ~== la2 && ln1 ~== ln2

newtype Radius r = Radius { getRadius :: Double }
  deriving (Show, Eq, Num, Fractional)

newtype Bearing = Bearing Double
  deriving (Show, Eq, Fractional, Num, AEq)

instance Arbitrary Bearing where
  arbitrary = do
    x <- choose (0, 359.9)
    pure (Bearing x)

newtype Distance = Distance { getDistance :: Double }
  deriving (Show, Eq, Fractional, Num, AEq)

instance Arbitrary Distance where
  arbitrary = do
    x <- choose (0,100)
    pure (Distance x)

data DistanceType = MILES | KILOMETRES
  deriving (Eq, Show)

class HasRadius (d :: DistanceType) where
  radius :: Double

instance HasRadius MILES where
  radius = earthRadiusMiles

instance HasRadius KILOMETRES where
  radius = earthRadiusKms

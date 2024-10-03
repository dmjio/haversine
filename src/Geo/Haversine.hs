{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
module Geo.Haversine
  ( -- * Types
    Coord (..)
  , Bearing (..)
  , Radius (..)
  , Distance (..)
  , DistanceType (..)
  -- * Classes
  , HasRadius (..)
  -- * API
  , haversine
  , reverseHaversine
  -- * Raw API
  , c_haversine
  , c_reverse_haversine
  , c_bearing
  -- * Constants
  , earthRadiusMiles
  , earthRadiusKms
  )

where

import Data.AEq         ((~==), AEq)
import Data.Proxy
import Foreign          (Ptr, alloca, peek)
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck  (Arbitrary)

earthRadiusMiles :: Double
earthRadiusMiles = 3963.1906

earthRadiusKms :: Double
earthRadiusKms = 6378.137

data Coord = Coord { getLat, getLng :: Double }
  deriving stock (Show, Eq)

instance Arbitrary Coord where
  arbitrary = do
    lat <- choose (-90, 90)
    lng <- choose (-180, 180)
    pure (Coord lat lng)

instance AEq Coord where
  Coord la1 ln1 ~== Coord la2 ln2 =
    la1 ~== la2 && ln2 ~== ln2

newtype Radius r = Radius { getRadius :: Double }
  deriving (Show, Eq, Num, Fractional)

newtype Bearing = Bearing Double
  deriving (Show, Eq, Fractional, Num)

newtype Distance = Distance { getDistance :: Double }
  deriving (Show, Eq)

data DistanceType = MILES | KILOMETRES
  deriving (Eq, Show)

class HasRadius (d :: DistanceType) where
  radius :: Double

instance HasRadius MILES where
  radius = earthRadiusMiles

instance HasRadius KILOMETRES where
  radius = earthRadiusKms

haversine :: forall r . HasRadius r => Coord -> Coord -> (Distance, Bearing)
haversine c1@(Coord lat1 lng1) c2@(Coord lat2 lng2) = unsafePerformIO $ do
  b <- bearing c1 c2
  d <- Distance <$> c_haversine lat1 lng1 lat2 lng2 (radius @r)
  pure (d, b)
  where
    bearing :: Coord -> Coord -> IO Bearing
    bearing (Coord lat1 lng1) (Coord lat2 lng2) =
      Bearing <$> c_bearing lat1 lng1 lat2 lng2

reverseHaversine :: forall r . HasRadius r => Coord -> Distance -> Bearing -> Coord
reverseHaversine (Coord lat1 lng1) (Distance d) (Bearing b) = unsafePerformIO $ do
  alloca $ \lat2 -> do
    alloca $ \lng2 -> do
      c_reverse_haversine lat1 lng1 b d lat2 lng2 (radius @r)
      Coord <$> peek lat2 <*> peek lng2

foreign import ccall "haversine.h haversine"
  c_haversine :: Double -> Double -> Double -> Double -> Double -> IO Double

foreign import ccall "haversine.h reverse_haversine"
  c_reverse_haversine :: Double -> Double -> Double -> Double -> Ptr Double -> Ptr Double -> Double -> IO ()

foreign import ccall "haversine.h bearing"
  c_bearing :: Double -> Double -> Double -> Double -> IO Double

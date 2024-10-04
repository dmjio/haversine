{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DataKinds                  #-}
module Geo.Haversine.Pure
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
  -- * Constants
  , earthRadiusMiles
  , earthRadiusKms
  ) where

import Data.Proxy
import Foreign          (Ptr, alloca, peek)
import System.IO.Unsafe

import Geo.Haversine.Types
import qualified Geo.Haversine as G

haversine :: forall r . HasRadius r => Coord -> Coord -> (Distance, Bearing)
haversine c1@(Coord lat1 lon1) c2@(Coord lat2 lon2) =
  let
     dLat = deg2rad (lat2 - lat1)
     dLon = deg2rad (lon2 - lon1)

     lat1' = deg2rad lat1
     lat2' = deg2rad lat2

     a = (sin(dLat / 2) ** 2) + (sin(dLon / 2) ** 2) * cos lat1' * cos lat2'
     c = 2 * (atan2 (sqrt a) (sqrt(1 - a)))
  in
     (Distance (radius @r * c), unsafePerformIO (G.bearing c1 c2))

reverseHaversine :: forall r . HasRadius r => Coord -> Distance -> Bearing -> Coord
reverseHaversine (Coord lat1 lon1) (Distance dist) (Bearing brng) =
  let
    lat1Rad = deg2rad lat1
    lon1Rad = deg2rad lon1
    brngRad = deg2rad brng
    lat2 = asin(sin(lat1Rad) * cos(dist / radius @r) + cos(lat1Rad) * sin(dist / radius @r) * cos(brngRad));
    lon2 = lon1Rad + atan2 (sin(brngRad) * sin (dist / radius @r) * cos(lat1Rad)) (cos(dist / radius @r) - sin(lat1Rad) * sin(lat2))
  in
    Coord (rad2deg lat2) (rad2deg lon2)

bearing :: Coord -> Coord -> Bearing
bearing (Coord lat1 lon1) (Coord lat2 lon2) =
  let
    lat1' = deg2rad lat1
    lon1' = deg2rad lon1
    lat2' = deg2rad lat2
    lon2' = deg2rad lon2
    y = sin(lon2 - lon1) * cos(lat2)
    x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon2 - lon1)
    bearing = rad2deg (atan2 y x)
  in
    if bearing < 0
      then Bearing (bearing + 360)
      else Bearing bearing

deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180.0

rad2deg :: Double -> Double
rad2deg rad = rad * 180.0 / pi


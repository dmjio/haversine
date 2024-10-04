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
  -- * Helpers
  , deg2rad
  , rad2deg
  ) where

import           Geo.Haversine.Types

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
     (Distance (radius @r * c), bearing c1 c2)

reverseHaversine :: forall r . HasRadius r => Coord -> Distance -> Bearing -> Coord
reverseHaversine (Coord lat1 lon1) (Distance dist) (Bearing brng) =
  let
    lat1' = deg2rad lat1
    lon1' = deg2rad lon1
    brng' = deg2rad brng
    lat2' = asin(sin(lat1') * cos(dist / radius @r) + cos(lat1') * sin(dist / radius @r) * cos(brng'))
    lon2' = lon1' + atan2 (sin(brng') * sin (dist / radius @r) * cos(lat1')) (cos(dist / radius @r) - sin(lat1') * sin(lat2'))
  in
    Coord (rad2deg lat2') (rad2deg lon2')

bearing :: Coord -> Coord -> Bearing
bearing (Coord lat1 lon1) (Coord lat2 lon2) =
  let
    lat1' = deg2rad lat1
    lon1' = deg2rad lon1
    lat2' = deg2rad lat2
    lon2' = deg2rad lon2
    y = sin(lon2' - lon1') * cos(lat2')
    x = cos(lat1') * sin(lat2') - sin(lat1') * cos(lat2') * cos(lon2' - lon1')
    brng = rad2deg (atan2 y x)
  in
    if brng < 0
      then Bearing (brng + 360)
      else Bearing brng

deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180.0

rad2deg :: Double -> Double
rad2deg rad = rad * 180.0 / pi


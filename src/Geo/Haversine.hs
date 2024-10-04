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
  , bearing
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

import Geo.Haversine.Types

haversine :: forall r . HasRadius r => Coord -> Coord -> (Distance, Bearing)
haversine c1@(Coord lat1 lng1) c2@(Coord lat2 lng2) = unsafePerformIO $ do
  b <- bearing c1 c2
  d <- Distance <$> c_haversine lat1 lng1 lat2 lng2 (radius @r)
  pure (d, b)

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

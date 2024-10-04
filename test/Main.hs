{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import           Data.AEq
import           Test.QuickCheck

import           Geo.Haversine
import qualified Geo.Haversine.Pure as P

import Numeric
import Text.Printf

-- | Test for approximate equality amongst both pure and impure implementations
main :: IO ()
main = do
  printf "%.5f\n" (P.deg2rad 1)
  printf "%.5f\n" (P.rad2deg (P.deg2rad 1))

  printf "%.5f\n" (deg2rad 1)
  printf "%.5f\n" (rad2deg (deg2rad 1))

  let nyc = Coord 40.7 74.006
      london = Coord 51.5074 0.1278

  -- let nyc = Coord {getLat = 41.90874154842831, getLng = 149.20305719574293}
  --     london = Coord {getLat = -32.69353401238422, getLng = -165.27279837678998}

  let (d1,b1) = haversine @KILOMETRES nyc london
  print $ haversine @KILOMETRES nyc london
  let london1 = reverseHaversine @KILOMETRES nyc d1 b1

  print ("nyc", nyc)
  print ("london", london)
  print ("london1", london1)

  print (london ~== london1)

  let (d1,b1) = P.haversine @KILOMETRES nyc london
  print $ P.haversine @KILOMETRES nyc london
  let london1 = P.reverseHaversine @KILOMETRES nyc d1 b1

  print ("nyc", nyc)
  print ("london", london)
  print ("london1", london1)

  print (london ~== london1)

  -- quickCheckWith stdArgs $ withMaxSuccess 100000 $ \(Coord lat lng) -> do
  --   lat ~== P.rad2deg (P.deg2rad lat) &&
  --     lng ~== P.rad2deg (P.deg2rad lng) &&
  --       lat ~== rad2deg (deg2rad lat) &&
  --         lng ~== rad2deg (deg2rad lng)

  -- quickCheckWith stdArgs $ withMaxSuccess 100000 $ \c1 c2 ->
  --   case (P.haversine @MILES c1 c2, haversine @MILES c1 c2) of
  --     ((d,b),(j,k)) ->
  --       case (reverseHaversine @MILES c1 d k, P.reverseHaversine @MILES c1 j b) of
  --         (c3,c4) -> and
  --           [ d ~== j
  --           , b ~== k
  --           , c2 ~== c3 && c3 ~== c4 && c2 ~== c4
  --           ]

  quickCheckWith stdArgs $ withMaxSuccess 100000 $ \c1 c2 ->
    case P.haversine @KILOMETRES c1 c2 of
      (d,b) ->
        case P.reverseHaversine @KILOMETRES c1 d b of
          c3 -> c2 ~== c3

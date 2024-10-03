{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.AEq
import Test.QuickCheck

import Geo.Haversine

main :: IO ()
main =
  quickCheckWith stdArgs $ withMaxSuccess 100000 $ \c1 c2 ->
    case haversine @MILES c1 c2 of
      (d,b) ->
        case reverseHaversine @MILES c1 d b of
          c3 -> c2 ~== c3

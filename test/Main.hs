{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import           Data.AEq
import           Test.QuickCheck

import           Geo.Haversine
import qualified Geo.Haversine.Pure as P

-- | Test for approximate equality amongst both pure and impure implementations
main :: IO ()
main = do
  quickCheckWith stdArgs $ withMaxSuccess 100000 $ \c1 c2 ->
    case (P.haversine @MILES c1 c2, haversine @MILES c1 c2) of
      ((d,b),(j,k)) ->
        case (reverseHaversine @MILES c1 d k, P.reverseHaversine @MILES c1 j b) of
          (c3,c4) -> and
            [ d ~== j
            , b ~== k
            , c3 ~== c4
            , c2 ~== c4
            ]

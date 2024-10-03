:compass: haversine
==================

Calculate distance in miles and kilometres between two coordinates. Also given an initial coordinate, distance and bearing, return the destination coordinate.

> The haversine formula determines the great-circle distance between two points on a sphere given their longitudes and latitudes. Important in navigation, it is a special case of a more general formula in spherical trigonometry, the law of haversines, that relates the sides and angles of spherical triangles.

## build

```bash
$ nix-build
$ nix-shell --run 'cabal build'
$ cabal build
```

## test

```bash
$ nix-shell --run 'cabal test'
$ cabal test
```

# example

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Data.AEq ((~==))

import Geo.Haversine

main :: IO ()
main = do
    let c1 = Coord 20 20
        c2 = Coord 30 30
        (distance, bearing) = haversine @MILES c1 c2
    let c3 = reverseHaversine @MILES c1 distance bearing
    print (c2 ~== c3)

-- True
```

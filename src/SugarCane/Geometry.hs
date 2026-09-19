module SugarCane.Geometry where

import Data.List ((!?))
import Data.Maybe

data Shape
  = Square {sideLength :: Int}
  | Rectangle {width :: Int, height :: Int}
  | Circle {diameter :: Int}
  | Ellipse {width :: Int, height :: Int}
  deriving stock (Show, Eq)

-- | Given some shape, determines if the corresponding coordinates lie within the shape's bounds.
contains :: Shape -> Int -> Int -> Bool
contains shape x y = case shape of
  Square {sideLength = s} -> contains Rectangle {width = s, height = s} x y
  Rectangle {width = w, height = h} -> (0 <= x && x < w) && (0 <= y && y < h)
  Circle {diameter = d} -> contains Ellipse {width = d, height = d} x y
  Ellipse {width = w, height = h} ->
    let hCentre = fromIntegral (w - 1) / 2 :: Float
        hSemiAxis = fromIntegral w / 2 :: Float
        hDelta = fromIntegral x - hCentre

        vCentre = fromIntegral (h - 1) / 2 :: Float
        vSemiAxis = fromIntegral h / 2 :: Float
        vDelta = fromIntegral y - vCentre

        hContrib = (hDelta * hDelta) / (hSemiAxis * hSemiAxis)
        vContrib = (vDelta * vDelta) / (vSemiAxis * vSemiAxis)
     in (hContrib + vContrib) <= 1

-- | The dimensions of the bounding box (smallest rectangle) surrounding the shape.
boxDimensions :: Shape -> (Int, Int)
boxDimensions shape = case shape of
  Square {sideLength = s} -> (s, s)
  Rectangle {width = w, height = h} -> (w, h)
  Circle {diameter = d} -> (d, d)
  Ellipse {width = w, height = h} -> (w, h)

------------------------------------------------------------
-- Grid Operations
------------------------------------------------------------

-- Index Generation

-- | Create a version of some grid with its indices exposed.
-- | Indices are zero-indexed.
indexGrid :: [[a]] -> [[(Int, Int, a)]]
indexGrid xss =
  zipWith
    ( \r row ->
        zipWith (\c val -> (c, r, val)) [0 ..] row
    )
    [0 ..]
    xss

-- | De-index a grid
deindex :: [[(Int, Int, a)]] -> [[a]]
deindex xss = [[val | (_, _, val) <- row] | row <- xss]

-- Adjacency Calculations

-- | Returns the neigbour at `offset`, if one exists.
neighbour :: [[a]] -> (Int, Int) -> (Int, Int) -> Maybe a
neighbour grid curr offset =
  let (x_1, z_1) = curr
      (x_2, z_2) = offset
      new_x = x_1 + x_2
      new_z = z_1 + z_2
   in grid !? new_z >>= (!? new_x)

-- | Returns a list of the immediate horizontal and vertical neighbours
-- | of the provided position.
neighbours :: [[a]] -> (Int, Int) -> [a]
neighbours grid curr = neighbours' grid curr [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- | Returns a list of the neigbhours at the given offsets.
neighbours' :: [[a]] -> (Int, Int) -> [(Int, Int)] -> [a]
neighbours' grid curr offsets =
  mapMaybe
    (\offset -> neighbour grid curr offset)
    offsets

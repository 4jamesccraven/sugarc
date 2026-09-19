------------------------------------------------------------------------------
-- sugarc -- Tools for obtaining optimal sugar cane farm layouts in Minecraft.
-- Copyright (C) 2026  James C. Craven <4jamesccraven@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
------------------------------------------------------------------------------

-- | Shape representation and grid manipulation.
module SugarCane.Geometry where

import Data.List ((!?))
import Data.Maybe

------------------------------------------------------------
-- Shape Operations
------------------------------------------------------------

-- | A geometric shape used to constuct a `Farm`.
data Shape
  = Square {sideLength :: Int}
  | Rectangle {width :: Int, height :: Int}
  | Circle {diameter :: Int}
  | Ellipse {width :: Int, height :: Int}
  deriving stock (Show, Eq)

-- | Given some shape, determines if the corresponding coordinates lie within the shape's bounds.
contains :: Shape -> Int -> Int -> Bool
contains shape x z = case shape of
  Square {sideLength = s} -> contains Rectangle {width = s, height = s} x z
  Rectangle {width = w, height = h} -> (0 <= x && x < w) && (0 <= z && z < h)
  Circle {diameter = d} -> contains Ellipse {width = d, height = d} x z
  Ellipse {width = w, height = h} ->
    let hCentre = fromIntegral (w - 1) / 2 :: Float
        hSemiAxis = fromIntegral w / 2 :: Float
        hDelta = fromIntegral x - hCentre

        vCentre = fromIntegral (h - 1) / 2 :: Float
        vSemiAxis = fromIntegral h / 2 :: Float
        vDelta = fromIntegral z - vCentre

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
--
-- Indices are zero-indexed and column major (@[z][x] <=> (x, z)@).
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

-- | Returns the neigbour at @offset@, if one exists.
neighbour :: [[a]] -> (Int, Int) -> (Int, Int) -> Maybe a
neighbour grid curr offset =
  let (x_1, z_1) = curr
      (x_2, z_2) = offset
      new_x = x_1 + x_2
      new_z = z_1 + z_2
   in grid !? new_z >>= (!? new_x)

-- | Returns a list of the immediate horizontal and vertical neighbours
-- of the provided position.
neighbours :: [[a]] -> (Int, Int) -> [a]
neighbours grid curr = neighbours' grid curr [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- | Returns a list of the neigbhours at the given offsets.
neighbours' :: [[a]] -> (Int, Int) -> [(Int, Int)] -> [a]
neighbours' grid curr offsets =
  mapMaybe
    (\offset -> neighbour grid curr offset)
    offsets

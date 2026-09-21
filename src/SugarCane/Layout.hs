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

-- | Representation and optimisation of a sugarcane farm in Minecraft.
module SugarCane.Layout where

import Data.List (elemIndices, intercalate)
import SugarCane.Farm qualified as Farm
import SugarCane.Geometry (deindex, indexGrid, neighbours)

-- | A single block in the farm.
data LayoutBlock
  = -- | A block that has water access and can thus support a sugarcane plant.
    Irrigated
  | -- | A block that lacks water access.
    Unirrigated
  | -- | A block of water.
    Water
  | -- | Any block that is not part of the farm/should be ignored.
    Blocked
  deriving stock (Show, Eq, Read, Enum)

-- | A higher-order representation of a `Farm` with finer-grained information
-- about its internal state.
data Layout = Layout
  { width :: Int,
    height :: Int,
    grid :: [[LayoutBlock]]
  }
  deriving stock (Eq)

instance Show Layout where
  show Layout {width = _, height = _, grid = grid} =
    let displayOne :: LayoutBlock -> String
        displayOne b = case b of
          Irrigated -> "\x1b[92m█\x1b[0m"
          Unirrigated -> "\x1b[93m▓\x1b[0m"
          Water -> "\x1b[94m░\x1b[0m"
          Blocked -> " "
     in intercalate "\n" $ [concat ([displayOne block | block <- row]) | row <- grid]

-----------------------------------------------------------
-- Layout Application
-----------------------------------------------------------

-- | A block is mutable if it is part of the Layout; i.e., if it is not ignored.
mutableBlock :: LayoutBlock -> Bool
mutableBlock b = b /= Blocked

-- | The phase of a coordinate pair in the farm.
--
-- The general optimal layout of a sugar cane farm is one where water is placed where
-- the following relationship holds, given some offset @c@:
-- @2x + z ≡ c (mod 5)@
phase :: Int -> Int -> Int
phase x z = (2 * x + z) `mod` 5

-- | Applies an optimal sugarcane pattern to a farm. Does not guarantee offset optimality.
applyLayout :: Int -> Layout -> Layout
applyLayout offset Layout {width = w, height = h, grid = blocks} =
  let withIndices = indexGrid blocks
      -- Apply water to the mutable blocks that match the desired phase.
      shouldBeWater :: Int -> Int -> LayoutBlock -> Bool
      shouldBeWater x z block = mutableBlock block && phase x z == offset

      withWater =
        [ [ (x, z, if shouldBeWater x z val then Water else val)
          | (x, z, val) <- row
          ]
        | row <- withIndices
        ]

      -- Mark blocks adjacent to water as irrigated
      waterGrid = deindex withWater
      shouldBeIrrigated :: Int -> Int -> LayoutBlock -> Bool
      shouldBeIrrigated x z block =
        block == Unirrigated
          && elem Water (neighbours waterGrid (x, z))

      withIrrigation =
        [ [ (x, z, if shouldBeIrrigated x z val then Irrigated else val)
          | (x, z, val) <- row
          ]
        | row <- withWater
        ]
   in Layout {width = w, height = h, grid = deindex withIrrigation}

-----------------------------------------------------------
-- Optimality
-----------------------------------------------------------

-- | Determine how optimal a layout is by counting all `Irrigated` blocks.
optimalityScore :: Layout -> Int
optimalityScore = optimalityScore' Irrigated

-- | Determine how optimal a layout is based on the count of an arbitrary
-- `LayoutBlock` state.
optimalityScore' :: LayoutBlock -> Layout -> Int
optimalityScore' state lay = length $ concatMap (filter (== state)) (grid lay)

-- | Finds the best offsets by optimising for the most possible irrigated blocks.
optimalOffsets :: Farm.Farm -> [Int]
optimalOffsets f =
  let allLayouts = layouts f
      allScores = map optimalityScore allLayouts
      maxScore = maximum allScores
   in elemIndices maxScore allScores

-- | Find all possible layouts for the farm.
layouts :: Farm.Farm -> [Layout]
layouts f =
  let farm = liftFarm f
   in map (`applyLayout` farm) [0 .. 4]

-- | Finds the optimal `Layout`s for this particular farm.
optimalLayout :: Farm.Farm -> [Layout]
optimalLayout f =
  let farm = liftFarm f
      offsets = optimalOffsets f
   in map (`applyLayout` farm) offsets

-----------------------------------------------------------
-- Conversions for `Farm` -> `Layout`
-----------------------------------------------------------

-- | Lifts a `FarmBlock` to a `LayoutBlock`
liftFarmBlock :: Farm.FarmBlock -> LayoutBlock
liftFarmBlock fb = case fb of
  Farm.Available -> Unirrigated
  Farm.Blocked -> Blocked

-- | Lifts an entire `Farm` to a `Layout`. This conversion is unoptimised.
--
-- All `Available` blocks are simply marked as `Unirrigated` pending
-- application of an actual, fully initialised layout.
liftFarm :: Farm.Farm -> Layout
liftFarm Farm.Farm {width = w, height = h, blocks = grid} =
  Layout
    { width = w,
      height = h,
      grid = [[liftFarmBlock b | b <- row] | row <- grid]
    }

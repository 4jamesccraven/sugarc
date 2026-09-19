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

-- | Basic representation of a farm.
--
-- This module is meant to give structure to a farm before considering
-- layout or optimisation thereof.
module SugarCane.Farm where

import Data.List (intercalate)
import SugarCane.Geometry

-- | Represents whether or not a block is part of the farm.
data FarmBlock
  = Available
  | Blocked
  deriving stock (Eq, Show, Read, Enum)

-- | The general layout/shape of a sugarcane farm.
data Farm = Farm
  { width :: Int,
    height :: Int,
    blocks :: [[FarmBlock]]
  }
  deriving stock (Eq)

instance Show Farm where
  show (Farm {width = _, height = _, blocks = grid}) =
    let displayOne :: FarmBlock -> String
        displayOne state = if state == Available then "█" else " "
     in intercalate "\n" $ map concat [[displayOne s | s <- row] | row <- grid]

-- | Creates a 2D list with the provided width and height. All entries start out `Available`.
farmGrid :: Int -> Int -> [[FarmBlock]]
farmGrid width height = replicate height $ replicate width Available

-- | Creates a farm of the provided shape.
shapedFarm :: Shape -> Farm
shapedFarm shape =
  let (width, height) = boxDimensions shape
      thisShapeContains = contains shape
      grid =
        [ [ if (thisShapeContains x y) then Available else Blocked
          | x <- [0 .. width - 1]
          ]
        | y <- [0 .. height - 1]
        ]
   in Farm {width = width, height = height, blocks = grid}

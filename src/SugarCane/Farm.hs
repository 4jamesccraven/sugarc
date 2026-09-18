module SugarCane.Farm where

import Data.List (intercalate)
import SugarCane.Geometry

-- | Mask atom for a layout.
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

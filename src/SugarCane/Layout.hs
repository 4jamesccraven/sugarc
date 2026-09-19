module SugarCane.Layout where

import Data.List (intercalate)
import SugarCane.Farm qualified as Farm
import SugarCane.Geometry (deindex, indexGrid, neighbours)

data LayoutBlock
  = Irrigated
  | Unirrigated
  | Water
  | Blocked
  deriving stock (Show, Eq, Read, Enum)

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
     in intercalate "\n" $ map concat [[displayOne block | block <- row] | row <- grid]

-----------------------------------------------------------
-- Layout Application
-----------------------------------------------------------

mutableBlock :: LayoutBlock -> Bool
mutableBlock b = b /= Blocked

phase :: Int -> Int -> Int
phase x z = (2 * x + z) `mod` 5

-- | Applies an optimal sugarcane pattern to a farm. Does not gurantee optimality.
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
          && any (\n -> n == Water) (neighbours waterGrid (x, z))
      withIrrigation =
        [ [ (x, z, if shouldBeIrrigated x z val then Irrigated else val)
          | (x, z, val) <- row
          ]
        | row <- withWater
        ]
   in Layout {width = w, height = h, grid = deindex withIrrigation}

-----------------------------------------------------------
-- Conversions for `Farm` -> `Layout`
-----------------------------------------------------------

-- | Lifts a `FarmBlock` to a `LayoutBlock`
liftFarmBlock :: Farm.FarmBlock -> LayoutBlock
liftFarmBlock fb = case fb of
  Farm.Available -> Unirrigated
  Farm.Blocked -> Blocked

-- | Lifts an entire `Farm` to a `Layout`
liftFarm :: Farm.Farm -> Layout
liftFarm Farm.Farm {width = w, height = h, blocks = grid} =
  Layout
    { width = w,
      height = h,
      grid = [[liftFarmBlock b | b <- row] | row <- grid]
    }

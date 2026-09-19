module SugarCane.Layout where

import Data.List (findIndices, intercalate)
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

-- | Applies an optimal sugarcane pattern to a farm. Does not guarantee optimality.
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
-- Optimality
-----------------------------------------------------------

-- | Determine how optimal a layout is by counting all `Irrigated` blocks.
optimalityScore :: Layout -> Int
optimalityScore lay = optimalityScore' Irrigated lay

-- | Determine how optimal a layout is based on the count of an arbitrary
-- | `LayoutBlock` state.
optimalityScore' :: LayoutBlock -> Layout -> Int
optimalityScore' state lay = length $ filter (\s -> s == state) (concat $ grid lay)

-- | Finds the best offsets by optimising for the most possible irrigated blocks.
optimalOffsets :: Farm.Farm -> [Int]
optimalOffsets f =
  let allLayouts = layouts f
      allScores = map optimalityScore allLayouts
      maxScore = maximum allScores
   in findIndices (\s -> s == maxScore) allScores

-- | Find all possible layouts for the farm.
layouts :: Farm.Farm -> [Layout]
layouts f =
  let farm = liftFarm f
   in map (\c -> applyLayout c farm) [0 .. 4]

-- | Finds the optimal `Layout`s for this particular farm.
optimalLayout :: Farm.Farm -> [Layout]
optimalLayout f =
  let farm = liftFarm f
      offsets = optimalOffsets f
   in map (\c -> applyLayout c farm) offsets

-----------------------------------------------------------
-- Conversions for `Farm` -> `Layout`
-----------------------------------------------------------

-- | Lifts a `FarmBlock` to a `LayoutBlock`
liftFarmBlock :: Farm.FarmBlock -> LayoutBlock
liftFarmBlock fb = case fb of
  Farm.Available -> Unirrigated
  Farm.Blocked -> Blocked

-- | Lifts an entire `Farm` to a `Layout`. This conversion is unoptimised.
liftFarm :: Farm.Farm -> Layout
liftFarm Farm.Farm {width = w, height = h, blocks = grid} =
  Layout
    { width = w,
      height = h,
      grid = [[liftFarmBlock b | b <- row] | row <- grid]
    }

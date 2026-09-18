module SugarCane.Geometry where

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

-- | ANSI escapes for coloured text.
--
-- An external lib is not used because whether or not coloured text will be
-- used long-term is uncertain, and it's not necessary for a hand-full of
-- colours.
module SugarCane.Internal.ANSI where

reset :: String
reset = "\x1b[0m"

greenFg :: String
greenFg = "\x1b[92m"

yellowFg :: String
yellowFg = "\x1b[93m"

blueFg :: String
blueFg = "\x1b[94m"

whiteBg :: String
whiteBg = "\x1b[47m"

blackFg :: String
blackFg = "\x1b[90m"

invertedFgBg :: String
invertedFgBg = blackFg ++ whiteBg

-- | Create a coloured span using ansi escapes.
colourSpan :: String -> String -> String
colourSpan effect text = effect ++ text ++ reset

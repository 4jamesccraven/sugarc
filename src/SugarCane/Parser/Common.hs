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

-- | Parsing functionality to convert input text to `ProgramInstruction`s.
--
-- Much of the design for this parser was inspired by Tsoding's pure-Haskell
-- JSON Parser. The referenced source code is available at:
--
-- https://github.com/tsoding/haskell-json/blob/8c5d63e37d67072b69eade833e141ce55ce44772/Main.hs
-- license: MIT
-- retrieved: 24 September, 2026
module SugarCane.Parser.Common where

import Control.Applicative
import Data.Char (isDigit, isSpace)
import SugarCane.Geometry
import SugarCane.Program

------------------------------------------------------------
-- Parser Type
------------------------------------------------------------

-- | Errors that can arise from the Parser.
data ParserError
  = EmptyError
  | ExpectedWhitespace
  | ExpectedChar Char
  | ExpectedToken String
  | ExpectedDigit
  | ExpectedShape
  | ExpectedGravity
  | ExpectedMaskArgs
  | CLIExpectedFlag
  | UnexpectedEOF
  deriving stock (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Either ParserError (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      pure (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (<*>) (Parser left) (Parser right) =
    Parser $ \input -> do
      (input', f) <- left input
      (input'', a) <- right input'
      pure (input'', f a)

instance Alternative (Either ParserError) where
  empty = Left EmptyError
  (<|>) (Left _) err = err
  (<|>) err _ = err

instance Alternative Parser where
  empty = Parser $ const empty
  (<|>) (Parser left) (Parser right) =
    Parser $ \input -> left input <|> right input

-- | Run a parser and replace its error if it fails.
mapErr :: Parser a -> ParserError -> Parser a
mapErr parser err = Parser $ \input ->
  case runParser parser input of
    Left _ -> Left err
    result -> result

------------------------------------------------------------
-- Parsers
------------------------------------------------------------

-- | Parser that expects a single character.
parseChar :: Char -> Parser Char
parseChar c = parseChar' (== c) (ExpectedChar c)

-- | Parser that parses a single character based on a predicate.
parseChar' :: (Char -> Bool) -> ParserError -> Parser Char
parseChar' predicate err = Parser fn
  where
    fn (c : cs)
      | predicate c = Right (cs, c)
      | otherwise = Left err
    fn _ = Left UnexpectedEOF

-- | Parser that expects a specific token.
parseToken :: String -> Parser String
parseToken str = Parser fn
  where
    fn input =
      case runParser (traverse parseChar str) input of
        Left _ -> Left $ ExpectedToken str
        result -> result

-- | Parser that expects a single digit.
parseDigit :: Parser Char
parseDigit = parseChar' isDigit ExpectedDigit

-- | Parser that creates a full integer.
parseInt :: Parser Int
parseInt = read <$> some parseDigit

-- | Parser that expects some amount of whitespace.
parseWhiteSpace :: Parser String
parseWhiteSpace = some $ parseChar' isSpace ExpectedWhitespace

-- | Parser that expects any amount of whitespace, including none.
skipWhiteSpace :: Parser String
skipWhiteSpace = many $ parseChar' isSpace EmptyError

-- | Parser that constructs a shape.
parseShape :: Parser Shape
parseShape =
  mapErr
    ( asum
        [ parseSquare,
          parseRectangle,
          parseCircle,
          parseEllipse
        ]
    )
    ExpectedShape
  where
    parseSquare =
      parseToken "square"
        *> parseWhiteSpace
        *> (Square <$> parseInt)

    parseRectangle =
      parseToken "rectangle"
        *> parseWhiteSpace
        *> (Rectangle <$> parseInt <*> (parseWhiteSpace *> parseInt))

    parseCircle =
      parseToken "circle"
        *> parseWhiteSpace
        *> (Circle <$> parseInt)

    parseEllipse =
      parseToken "ellipse"
        *> parseWhiteSpace
        *> (Ellipse <$> parseInt <*> (parseWhiteSpace *> parseInt))

-- | Parser that constructs a gravity value.
parseGravity :: Parser Gravity
parseGravity =
  mapErr
    ( parseCombined
        <|> (Horizontal <$> parseHorizontal)
        <|> (Vertical <$> parseVertical)
    )
    ExpectedGravity
  where
    parseSep = parseToken "|" <|> parseToken "+"

    parseCombined =
      Combined
        <$> parseVertical
        <*> (skipWhiteSpace *> parseSep *> skipWhiteSpace *> parseHorizontal)

    parseHorizontal =
      AlignLeft <$ parseToken "left"
        <|> (AlignCentre <$ (parseToken "centre" <|> parseToken "center"))
        <|> (AlignRight <$ parseToken "right")

    parseVertical =
      AlignTop <$ parseToken "top"
        <|> (AlignHorizon <$ parseToken "horizon")
        <|> (AlignBottom <$ parseToken "bottom")

-- | Parser that constructs arguments for masking operations.
parseMaskArgs :: Parser MaskArgs
parseMaskArgs =
  mapErr
    (parseMAlign <|> parseGAlign <|> parseSAlign)
    ExpectedMaskArgs
  where
    parseMAlign =
      AlignManual
        <$> parseShape
        <*> ( skipWhiteSpace
                *> parseToken ","
                *> skipWhiteSpace
                *> ((,) <$> parseInt <*> (parseWhiteSpace *> parseInt))
            )

    parseGAlign = AlignGravity <$> parseShape

    parseSAlign =
      AlignSingular
        <$> ((,) <$> parseInt <*> (parseWhiteSpace *> parseInt))

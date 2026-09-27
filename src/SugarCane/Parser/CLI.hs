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
module SugarCane.Parser.CLI where

import Control.Applicative
import Data.List (intercalate)
import SugarCane.Parser.Common
import SugarCane.Program
import System.Environment (getArgs)

-- | What the sugarc program should ultimately do.
data CLIFinalBehaviour
  = CLIPrintHelp
  | CLIPrintVersion
  | CLIRunProgram [ProgramInstruction]
  deriving stock (Show, Eq)

-- | Runs the CLI Parser.
runCliParser :: IO (Either ParserError CLIFinalBehaviour)
runCliParser = do
  args <- getArgs
  pure $ runCliParser' $ intercalate " " args

-- | Parses the CLI from a set of args.
runCliParser' :: String -> Either ParserError CLIFinalBehaviour
runCliParser' args = snd <$> runParser parseCLI args

-- | Create a parser that parses the entire CLI.
parseCLI :: Parser CLIFinalBehaviour
parseCLI =
  foldl1
    (<|>)
    [ (CLIPrintHelp <$ (parseToken "help" <|> parseFlag "help")),
      (CLIPrintHelp <$ (parseToken "version" <|> parseFlag "version")),
      (CLIRunProgram <$> parseProgram)
    ]
  where
    parseProgramStart = ((: []) . From) <$> (parseToken "from" *> skipWhiteSpace *> parseShape)
    parseProgram = (++) <$> parseProgramStart <*> many parseNextOpt <* parseEof
      where
        parseNextOpt = skipWhiteSpace *> peekChar (== '-') *> parseOpt

-- | Parser that parses a named CLI flag.
--
-- A both a short form and long form are accepted. To only parse a long flag,
-- use `parseFlag'`
parseFlag :: String -> Parser String
parseFlag name = parseFlag' name <|> parseFlag' (take 1 name)

-- | Parser that parses a named CLI flag.
parseFlag' :: String -> Parser String
parseFlag' name = parseToken ("-" ++ name)

-- | Parser that parses the value after a flag has been successfully parsed.
parseVal :: Parser a -> Parser a
parseVal p = commit (skipWhiteSpace *> p)

-- | Parser that parses the type of output for the program.
parseEmission :: Parser EmissionType
parseEmission = Stdout <$ (parseToken "stdout" <|> parseToken "-")

-- | Parser that parses any command line option into an instruction.
parseOpt :: Parser ProgramInstruction
parseOpt =
  mapErr
    ( asum
        [ parseOptG,
          parseOptB,
          parseOptA,
          parseOptE,
          parseOptT
        ]
    )
    CLIExpectedFlag
  where
    parseOptG = SetGravity <$> (parseFlag "gravity" *> parseVal parseGravity)
    parseOptB = Block <$> (parseFlag "block" *> parseVal parseMaskArgs)
    parseOptA = Allow <$> (parseFlag "allow" *> parseVal parseMaskArgs)
    parseOptE = Emit <$> (parseFlag "emit" *> parseVal parseEmission)
    parseOptT =
      parseFlag "take"
        *> parseVal
          ( asum
              [ (AllOfThem <$ parseToken "all"),
                (Whichever <$ parseToken "one"),
                (AllOptimal <$ parseToken "any"),
                (OnlyFarms <$ parseToken "farm"),
                (PickLayout <$> parseInt)
              ]
          )

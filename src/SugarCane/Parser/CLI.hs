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

-- | Parsing functionality specific to the CLI
module SugarCane.Parser.CLI where

import Control.Applicative
import SugarCane.Parser.Common
import SugarCane.Program
import System.Environment (getArgs)

-- | What the sugarc program should ultimately do.
data CLIFinalBehaviour
  = CLIPrintHelp
  | CLIPrintVersion
  | CLIRunProgram [ProgramInstruction]
  | CLIExitHelp
  deriving stock (Show, Eq)

------------------------------------------------------------
-- Top Level Runners
------------------------------------------------------------

-- | Runs the CLI Parser.
runCliParser :: IO (Either ParserError CLIFinalBehaviour)
runCliParser = do runCliParser' . unwords <$> getArgs

-- | Parses the CLI from a set of args.
runCliParser' :: String -> Either ParserError CLIFinalBehaviour
runCliParser' args = case snd <$> runParser parseCLI args of
  Left (ExpectedToken "from") -> Right CLIExitHelp
  result -> result

------------------------------------------------------------
-- Parsers
------------------------------------------------------------

-- | Create a parser that parses the entire CLI.
parseCLI :: Parser CLIFinalBehaviour
parseCLI =
  foldl1
    (<|>)
    [ CLIPrintHelp <$ parseSubcommand "help",
      CLIPrintHelp <$ parseSubcommand "version",
      CLIRunProgram <$> parseProgram
    ]
  where
    parseProgramStart = (: []) . From <$> (parseSubcommand "from" *> skipWhiteSpace *> parseShape)
    parseProgram = (++) <$> parseProgramStart <*> many parseNextOpt <* parseEof
      where
        parseNextOpt = skipWhiteSpace *> peekChar (== '-') *> parseOpt

-- | Parses a top-level command for the program.
--
-- The most permissive flag parser, allows ffmpeg-like, UNIX long and short,
-- and subcommand styles (e.g., @help@, @--help@, @-help@, and @-h@ are all
-- accepted).
parseSubcommand :: String -> Parser String
parseSubcommand name =
  mapErr
    ( parseToken name
        <|> parseToken ("--" ++ name)
        <|> parseFlag name
    )
    CLIExpectedSubcommand

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
parseVal :: String -> Parser a -> Parser a
parseVal ctx p = commit ctx (skipWhiteSpace *> p)

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
    optParser :: String -> Parser a -> Parser a
    optParser name parser = parseFlag name *> parseVal ("arguments to flag `-" ++ name ++ "`") parser
    parseOptG = SetGravity <$> optParser "gravity" parseGravity
    parseOptB = Block <$> optParser "block" parseMaskArgs
    parseOptA = Allow <$> optParser "allow" parseMaskArgs
    parseOptE = Emit <$> optParser "emit" parseEmission
    parseOptT =
      optParser
        "take"
        ( asum
            [ AllOfThem <$ parseToken "all",
              Whichever <$ parseToken "one",
              AllOptimal <$ parseToken "any",
              OnlyFarms <$ parseToken "farm",
              PickLayout <$> parseInt
            ]
        )

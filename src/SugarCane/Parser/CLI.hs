module SugarCane.Parser.CLI where

import Control.Applicative
import Data.Char (toLower)
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
  pure $ runCliParser' args

-- | Parses the CLI from a set of args.
runCliParser' :: [String] -> Either ParserError CLIFinalBehaviour
runCliParser' args =
  let joined = intercalate " " args
      cli = map toLower joined
   in snd <$> runParser parseCLI cli

-- | Create a parser that parses the entire CLI.
parseCLI :: Parser CLIFinalBehaviour
parseCLI =
  asum
    [ (CLIPrintHelp <$ (parseToken "help" <|> parseFlag "help")),
      (CLIPrintHelp <$ (parseToken "version" <|> parseFlag "version")),
      (CLIRunProgram <$> parseProgram)
    ]
  where
    parseProgramStart = ((: []) . From) <$> (parseToken "from" *> skipWhiteSpace *> parseShape)
    parseProgram = (++) <$> parseProgramStart <*> many (skipWhiteSpace *> parseOpt)

-- | Parser that parses a named CLI flag.
--
-- A both a short form and long form are accepted. To only parse a long flag,
-- use `parseFlag'`
parseFlag :: String -> Parser String
parseFlag name = parseFlag' name <|> parseFlag' (take 1 name)

-- | Parser that parses a named CLI flag.
parseFlag' :: String -> Parser String
parseFlag' name = parseToken ("-" ++ name)

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
    parseOptG = SetGravity <$> (parseFlag "gravity" *> skipWhiteSpace *> parseGravity)
    parseOptB = Block <$> (parseFlag "block" *> skipWhiteSpace *> parseMaskArgs)
    parseOptA = Allow <$> (parseFlag "allow" *> skipWhiteSpace *> parseMaskArgs)
    parseOptE = Emit <$> (parseFlag "emit" *> skipWhiteSpace *> parseEmission)
    parseOptT =
      parseFlag "take"
        *> skipWhiteSpace
        *> ( asum
               [ (AllOfThem <$ parseToken "all"),
                 (Whichever <$ parseToken "one"),
                 (AllOptimal <$ parseToken "any"),
                 (OnlyFarms <$ parseToken "farm"),
                 (PickLayout <$> parseInt)
               ]
           )

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

-- | Entry point for the @sugarc@ executable.
module Main (main) where

import SugarCane.Parser.CLI
import SugarCane.Program

-- | Ensures that at least one take instruction is present
-- (default: All Optimal Layouts)
ensureTake :: [ProgramInstruction] -> [ProgramInstruction]
ensureTake instructions
  | any isTake instructions = instructions
  | otherwise = instructions ++ [AllOptimal]
  where
    isTake instruction = case instruction of
      Whichever -> True
      AllOfThem -> True
      AllOptimal -> True
      OnlyFarms -> True
      PickLayout _ -> True
      _ -> False

main :: IO ()
main = do
  doWhat <- runCliParser
  case doWhat of
    Right CLIPrintHelp -> putStrLn "TODO"
    Right CLIExitHelp -> putStrLn "TODO (but bad)"
    Right CLIPrintVersion -> putStrLn "TODO"
    Right (CLIRunProgram parsed) ->
      let instructions = ensureTake parsed
       in case runProgram instructions of
            Right state -> putStrLn $ show $ result state
            Left err -> putStrLn $ show $ err
    Left err -> putStrLn $ show $ err
  pure ()

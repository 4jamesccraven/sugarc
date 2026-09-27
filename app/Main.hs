module Main (main) where

import SugarCane.Parser.CLI
import SugarCane.Program

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

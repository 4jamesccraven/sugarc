module SugarCane.Program where

import Data.List (intercalate)
import SugarCane.Farm
import SugarCane.Geometry
import SugarCane.Layout

------------------------------------------------------------
-- Program State Representation
------------------------------------------------------------

-- | The state of the sugarc program.
data ProgramState = ProgramState
  { gravity :: Gravity,
    emission :: EmissionType,
    result :: ProgramResult
  }

instance Show ProgramState where
  show (ProgramState {gravity = gravity, emission = emission, result = result}) =
    "ProgramState where\n"
      ++ "gravity = "
      ++ show gravity
      ++ "\n"
      ++ "emission = "
      ++ show emission
      ++ "\n"
      ++ "result = \n"
      ++ show result
      ++ "\n"

defaultProgramState :: ProgramState
defaultProgramState =
  ProgramState {gravity = alignTrueCentre, emission = Stdout, result = NothingYet}

-- | The final output the program builds.
--
-- As a program runs, it may or may not reach the point of a final, singular
-- layout depending on several factors (e.g., the user not selecting a layout
-- from those available, or deciding to only model a farm without optimising
-- it). Such cases are not errors so the final result is modelled as the
-- various forms it can take.
data ProgramResult
  = NothingYet
  | FarmState Farm
  | Layouts [Layout]
  | FinalLayout Layout

instance Show ProgramResult where
  show result = case result of
    NothingYet -> "undefined"
    FarmState f -> show f
    Layouts ls -> intercalate "\n\n" $ map layoutReport ls
    FinalLayout l -> layoutReport l

-- | How the output should be presented.
--
-- For now, stdout is the only option.
data EmissionType
  = Stdout
  deriving stock (Show, Eq)

------------------------------------------------------------
-- Sugarc Intermediate Representation
------------------------------------------------------------

-- | The smallest definable action an external program can take.
--
-- This is an IR for a sugarc "program." This does not define all
-- possible external APIs. For example, the CLI simplifies all
-- positive masking operations into one flag.
-- TODO: Make sure I actually do this when I implement CLI ^ !!!
{- ORMOLU_DISABLE -}
data ProgramInstruction
  =
    --- State Manipulation ---
    -- | Defines the starting shape of a farm.
    From Shape
  | -- | Sets the gravity state for the program.
    SetGravity Gravity
  | -- | Defines the output type.
    Emit EmissionType

    --- Negative Mask Operations ---
  | -- | Removes a shape. Positioned based on current `Gravity`
    GravityBlock Shape
  | -- | Removes a shape. Top-left position must be provided.
    Block Shape (Int, Int)
  | -- | Removes a single block at the provided coordinates.
    PrecisionBlock (Int, Int)
  |

    --- Positive Mask Operations ---
    -- | Adds a shape. Positioned based on current `Gravity`
    GravityAllow Shape
  | -- | Adds a shape. Top-left position must be provided.
    Allow Shape (Int, Int)
  | -- | Adds a single block at the provided coordinates.
    PrecisionAllow (Int, Int)
  |

    --- Program Returns ---
    -- | Disregard equivalently offset farms. In other words, "just pick one, bro."
    Whichever
  | -- | Takes all layouts, regardless of optimality.
    AllOfThem
  | -- | Takes all optimal layouts.
    AllOptimal
  | -- | Picks a specific phase
    PickLayout (Int)
{- ORMOLU_ENABLE -}

------------------------------------------------------------
-- Control Flow
------------------------------------------------------------

-- | Ways that a sugarc program can fail.
data ProgramError
  = MultipleFrom
  | MissingFarm
  | InvalidPhase Int

instance Show ProgramError where
  show perror =
    let msg = case perror of
          MultipleFrom -> "FROM directive applied more than once"
          MissingFarm -> "no farm has been initialised"
          InvalidPhase i -> "invalid phase value: " ++ show i ++ ". the phase has to be a whole number in [0, 4]"
     in "ERROR: " ++ msg

-- | The result of a single step of the interpreter. Models whether the program
-- is complete at the current state.
data StepResult
  = Continue ProgramState
  | Exit ProgramState

--- Convenience Wrappers ---
continue :: ProgramState -> Either ProgramError StepResult
continue = Right . Continue

exit :: ProgramState -> Either ProgramError StepResult
exit = Right . Exit

die :: ProgramError -> Either ProgramError StepResult
die = Left

-- | Transforms the program state's farm if and only if it contains a farm.
-- Returns a `MissingFarm` if the result state is any of (`NothingYet`,
-- `Layouts`, `FinalLayout`)
withFarm :: ProgramState -> (Farm -> Farm) -> Either ProgramError StepResult
withFarm state@ProgramState {result = FarmState f} transform =
  continue $ state {result = FarmState $ transform f}
withFarm _ _ = die MissingFarm

-- | Performs a final transformation on the farm before exiting the program.
-- Returns a `MissingFarm` if the result state is any of (`NothingYet`,
-- `Layouts`, `FinalLayout`)
exitAs :: ProgramState -> (Farm -> ProgramResult) -> Either ProgramError StepResult
exitAs state@ProgramState {result = FarmState f} transform =
  exit $ state {result = transform f}
exitAs _ _ = die MissingFarm

------------------------------------------------------------
-- Program Interpretation
------------------------------------------------------------

-- | Runs a single instruction on the program state.
runInstruction :: ProgramState -> ProgramInstruction -> Either ProgramError StepResult
runInstruction state@ProgramState {gravity = gravity, result = result} instruction = case instruction of
  --- State Manipulation ---
  From s -> case result of
    NothingYet -> continue $ state {result = FarmState $ shapedFarm s}
    _ -> die MultipleFrom
  SetGravity g -> continue $ state {gravity = g}
  Emit e -> continue $ state {emission = e}
  --- Negative Masks ---
  GravityBlock s ->
    withFarm state (blockGravity gravity s)
  Block s pos ->
    withFarm state (blockMask s pos)
  PrecisionBlock pos ->
    withFarm state (blockOne pos)
  --- Positive Masks ---
  GravityAllow s ->
    withFarm state (allowGravity gravity s)
  Allow s pos ->
    withFarm state (allowMask s pos)
  PrecisionAllow pos ->
    withFarm state (allowOne pos)
  --- Program Returns
  Whichever ->
    exitAs state (FinalLayout . (!! 0) . optimalLayout)
  AllOfThem ->
    exitAs state (Layouts . layouts)
  AllOptimal ->
    exitAs state (Layouts . optimalLayout)
  PickLayout i
    | 0 <= i && i <= 4 -> exitAs state (FinalLayout . (!! i) . layouts)
    | otherwise -> die $ InvalidPhase i

-- | Runs a full program by applying each instruction, stopping when
-- `Exit` is encountered or when there are no more `ProgramInstruction`s.
runProgram' ::
  ProgramState ->
  [ProgramInstruction] ->
  Either ProgramError ProgramState
runProgram' state [] = Right state
runProgram' state (instruction : rest) =
  runInstruction state instruction >>= \case
    Continue nextState -> runProgram' nextState rest
    Exit finalState -> Right finalState

-- | Run a program with default state.
runProgram :: [ProgramInstruction] -> Either ProgramError ProgramState
runProgram = runProgram' defaultProgramState

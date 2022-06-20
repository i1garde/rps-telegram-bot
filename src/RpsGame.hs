module RpsGame (RPS(..), RoundOutcome(..), runGame, printGameResult) where
import Control.Monad.State

data RPS = Rock | Paper | Scissors
  deriving (Eq, Enum, Bounded, Show)

type GameState = (RPS, RPS)

instance Ord RPS where
  compare Rock Rock = EQ
  compare Paper Paper = EQ
  compare Scissors Scissors = EQ
  compare Rock Paper = LT
  compare Paper Rock = GT
  compare Rock Scissors = GT
  compare Scissors Rock = LT
  compare Paper Scissors = LT
  compare Scissors Paper = GT

data RoundOutcome = Win | Lose | Draw
  deriving (Eq, Show)

class GameLogic repr where
  rps :: RPS -> repr RPS
  roundOutcome :: RoundOutcome -> repr RoundOutcome
  getRoundRes :: repr RPS -> repr RPS -> repr RoundOutcome

comapreRPS :: (GameLogic a) => RPS -> RPS -> a RoundOutcome
comapreRPS i1 i2 = 
  case comp of GT -> roundOutcome Win
               EQ -> roundOutcome Draw
               LT -> roundOutcome Lose
  where comp = compare i1 i2

newtype G a = G { unG :: a }

instance GameLogic G where
  rps = G
  roundOutcome = G
  getRoundRes i1 i2 =
    let comp = compare (unG i1) (unG i2)
    in case comp of GT -> G Win
                    EQ -> G Draw
                    LT -> G Lose

evalGame :: G a -> a
evalGame = unG

newtype SI a = SI { unSI :: State GameState a }

instance GameLogic SI where
  rps = SI . return
  roundOutcome = SI . return
  getRoundRes si1 si2 = SI $ do
    i1 <- unSI si1 
    i2 <- unSI si2
    put (i1, i2)
    unSI $ comapreRPS i1 i2

evalSGameLogic :: SI a -> State GameState a
evalSGameLogic = unSI

runGameState :: State GameState a -> (a, GameState)
runGameState state = runState state (Paper, Paper)

evalGameState :: State GameState a -> a 
evalGameState state = evalState state (Paper, Paper)

execGameState :: State GameState a -> GameState
execGameState state = execState state (Paper, Paper)

runGame :: RPS -> RPS -> (RoundOutcome, (RPS, RPS))
runGame i1 i2 = runGameState $ evalSGameLogic $ getRoundRes (rps i1) (rps i2)

printGameResult :: (RoundOutcome, (RPS, RPS)) -> String
printGameResult (ro, (i1, i2)) = "Game result: " ++ show ro ++ "\n" ++
  "Your weapon: " ++ show i1 ++ "\n" ++
  "Bot's weapon: " ++ show i2 ++ "\n"
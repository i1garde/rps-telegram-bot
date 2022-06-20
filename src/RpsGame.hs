module RpsGame (RPS(..), RoundOutcome(..), GameState, runGame) where
import Control.Monad.State

data RPS = Rock | Paper | Scissors
  deriving (Eq, Enum, Bounded, Show)

type ItemsState = (RPS, RPS)

type GameState = (ItemsState, RoundOutcome)

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

--

class GameLogic repr where
  rps :: RPS -> repr RPS
  roundOutcome :: RoundOutcome -> repr RoundOutcome
  getRoundRes :: repr RPS -> repr RPS -> repr GameState

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
    in case comp of GT -> G ((unG i1, unG i2), Win)
                    EQ -> G ((unG i1, unG i2), Draw)
                    LT -> G ((unG i1, unG i2), Lose)

evalGame :: G a -> a
evalGame = unG

runGame :: RPS -> RPS -> GameState
runGame i1 i2 = evalGame $ getRoundRes (rps i1) (rps i2)

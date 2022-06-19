module RpsGame where

data RPS = Rock | Paper | Scissors
  deriving (Eq, Enum, Bounded, Show)

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
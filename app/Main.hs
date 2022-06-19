module Main where

import RpsGame
import RpsRandom

main :: IO ()
main = do
  item <- randomRPS
  putStrLn "Choose your weapon: \n1) Rock;\n2) Paper;\n3) Scissors;"
  weaponId <- getLine
  case weaponId of
    "1" ->
      print (evalGameState $ trackState Rock item) >>
      putStrLn "(Your weapon, enemy's weapon)" >>
      print (execGameState $ trackState Rock item)
    "2" -> 
      print (evalGameState $ trackState Paper item) >>
      putStrLn "(Your weapon, enemy's weapon)" >>
      print (execGameState $ trackState Paper item)
    "3" -> 
      print (evalGameState $ trackState Scissors item) >>
      putStrLn "(Your weapon, enemy's weapon)" >>
      print (execGameState $ trackState Scissors item)
    _ -> error "Wrong weapon."
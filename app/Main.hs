module Main where

import RpsGame
import RpsRandom

main :: IO ()
main = do
  item <- randomItem
  putStrLn "Choose your weapon: \n1) Rock;\n2) Paper;\n3) Scissors;"
  weaponId <- getLine
  case weaponId of
    "1" ->
      putStrLn . printGameResult $ runGame Rock item
    "2" -> 
      putStrLn . printGameResult $ runGame Paper item
    "3" -> 
      putStrLn . printGameResult $ runGame Scissors item
    _ -> error "Wrong weapon."

printGameResult :: GameState -> String
printGameResult ((i1, i2), ro) = "Game result: " ++ show ro ++ "\n" ++
  "Your weapon: " ++ show i1 ++ "\n" ++
  "Bot's weapon: " ++ show i2 ++ "\n"
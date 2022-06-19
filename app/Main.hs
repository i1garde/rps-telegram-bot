module Main where

import RpsGame
import RpsRandom

main :: IO ()
main = do
  item <- randomRPS
  putStrLn "Choose your weapon: \n1) Rock;\n2) Paper;\n3) Scissors;"
  weaponId <- getLine
  case weaponId of
    "1" -> print . evalGame $ getRoundRes (rps Rock) (G item)
    "2" -> print . evalGame $ getRoundRes (rps Paper) (G item)
    "3" -> print . evalGame $ getRoundRes (rps Scissors) (G item)
    _ -> error "Wrong weapon." 

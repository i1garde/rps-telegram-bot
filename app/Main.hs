module Main where

import RpsGame
import RpsRandom ( randomItem )
import qualified Data.Text as Text
import Telegram.Bot.API
import Bot (run)

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
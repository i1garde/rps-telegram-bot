{-# LANGUAGE OverloadedStrings #-}

module Bot where
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import RpsGame (GameState, RPS (..), runGame)
import Telegram.Bot.Simple.UpdateParser
import Control.Applicative
import Control.Monad.IO.Class
import RpsRandom (randomItem)
import Data.Text (pack)

type Model = ()

data Action
  = -- | Perform no action.
    NoAction
  | Start
  | Help
  | ChooseGameItem RPS
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = ()
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ =
  parseUpdate
    ( Start <$ command "start"
        <|> Help <$ command "help"
        <|> ChooseGameItem Rock <$ command "rock"
        <|> ChooseGameItem Paper <$ command "paper"
        <|> ChooseGameItem Scissors <$ command "scissors"
    )

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  Start ->
    model <# do
      replyOrEdit returnStartMessage
      pure NoAction
  Help ->
    model <# do
      replyOrEdit returnHelpMessage
      pure NoAction
  ChooseGameItem gameItem ->
    model <# do
      randItem <- liftIO randomItem
      replyText . pack . show $ runGame gameItem randItem
      --replyOrEdit . returnResultMessage $ "Hello"
      --editUpdateMessage . returnResultMessage $ gameplay gameItem Scissors
      pure NoAction

returnStartMessage :: EditMessage
returnStartMessage =
  toEditMessage "Welcome to the Rock Paper Scissors game! (/help to discover commands)"

returnHelpMessage :: EditMessage
returnHelpMessage =
  toEditMessage "Choose Rock, Paper or Scissors: ( /rock , /paper , /scissors )"

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

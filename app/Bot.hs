{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import RpsGame (GameState, RPS (..), runGame, RoundOutcome (..))
import Telegram.Bot.Simple.UpdateParser
import Control.Applicative
import Control.Monad.IO.Class
import RpsRandom (randomItem)
import Data.Text (pack, Text, append)
import Telegram.Bot.API
import Data.Text.Internal.Lazy (Text(Empty))
import qualified GHC.TypeLits as Data
import Fmt
import Fmt.Internal.Core (FromBuilder(fromBuilder))

type Model = ()

data Action
  = -- | Perform no action.
    NoAction
  | Start
  | Help
  | ChooseGameItem RPS
  | ActMessage Data.Text.Text
  deriving (Show)

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = ()
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ =
  parseUpdate
    ( Start <$ command "start"
        <|> Help <$ command "help"
        <|> ActMessage <$> plainText
    )

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  Help ->
    model <# do
      replyOrEdit returnHelpMessage
      pure NoAction
  Start -> model <# do
      reply (toReplyMessage "Let's play!\nChoose your item: ")
        { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
  ActMessage msg -> model <# do
    botItem <- liftIO randomItem
    case msg of
      "Rock" -> replyOrEdit $ toEditMessage $ formatResult $ runGame Rock botItem
      "Paper" -> replyOrEdit $ toEditMessage $ formatResult $ runGame Paper botItem
      "Scissors" -> replyOrEdit $ toEditMessage $ formatResult $ runGame Scissors botItem
      --_ -> reply (toReplyMessage msg) 

formatRoundOutcome :: RoundOutcome -> String
formatRoundOutcome roundOutcome =
  case roundOutcome of
    Win -> "You win!"
    Lose -> "Bot wins."
    Draw -> "It's a draw."

formatResult :: GameState -> Data.Text.Text
formatResult ((item, botItem), roundOutcome) = "You choosed " +|| item ||+ "\nBot's item is " +|| botItem ||+"\n" +|| formatRoundOutcome roundOutcome ||+ ""

replyString :: String -> BotM ()
replyString = reply . toReplyMessage . pack

returnHelpMessage :: EditMessage
returnHelpMessage =
  toEditMessage "To start Rock, Paper or Scissors game round type /start"

startKeyboard :: ReplyKeyboardMarkup
startKeyboard = ReplyKeyboardMarkup
  { replyKeyboardMarkupKeyboard =
      [ [ "Rock" ]
      , [ "Paper"]
      , [ "Scissors" ]
      ]
  , replyKeyboardMarkupResizeKeyboard = Just True
  , replyKeyboardMarkupOneTimeKeyboard = Just True
  , replyKeyboardMarkupSelective = Nothing
  , replyKeyboardMarkupInputFieldSelector = Nothing
  }

-- | Run bot with a given 'Telegram.Token'.
run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

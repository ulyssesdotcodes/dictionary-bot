{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import BasePrelude hiding (intercalate, filter)
import Control.Lens ((^.), (^?))
import Control.Lens.Getter (Getting)
import Data.Aeson ((.:), (.:?), decode, encode, FromJSON(..), Value(..), fromJSON)
import Data.Aeson.Lens (key, values)
import Data.Attoparsec.Text.Lazy
import Data.Char (isLetter, isAscii)
import Data.Map (Map)
import Data.Text.Lazy hiding (span, drop, words)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Internal as TS
import Data.ByteString.Lazy.Internal (ByteString)
import GHC.Generics (Generic)
import Network.Linklater (say, slashSimple, Command(..), Config(..), Message(..), Icon(..), Format(..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)

getWordResponse :: Text -> IO (Response ByteString)
getWordResponse word = do
    r <- get ("https://api.pearson.com/v2/dictionaries/entries?headword=" <> unpack word)
    return r

getDefinition :: Response ByteString -> Maybe Value
getDefinition response = 
    (response ^? responseBody . key "results" . values . key "senses" . values . key "definition")

getPartOfSpeech :: Response ByteString -> Maybe Value
getPartOfSpeech response =
    (response ^? responseBody . key "results" . values . key "part_of_speech")

valueToString :: Value -> TS.Text
valueToString (String value) =
    value

createDefinition :: (Maybe TS.Text, Maybe TS.Text) -> Maybe Text
createDefinition (Just pos, Just def) =
    Just $ fromStrict pos <> ". " <> fromStrict def
createDefinition (Nothing, Just def) =
    Just $ fromStrict def
createDefinition (_, Nothing) = Nothing

getFullDefinition :: IO (Response ByteString) -> IO (Maybe Text)
getFullDefinition response = do
    r <- response
    return (createDefinition (fmap valueToString $ getPartOfSpeech r, fmap valueToString $ getDefinition r))

define :: Text -> IO Text
define checkWord = do
    w <- definition
    case w of
      Just v -> return (checkWord <> " - " <> v)
      Nothing -> return "something went wrong"
    where 
        definition = getFullDefinition $ getWordResponse checkWord

dict :: Maybe Command -> IO Text
dict (Just (Command user channel (Just text))) =
  define text
  -- where 
  --  config' = (Config "trello.slack.com" . filter (/= '\n') . pack) <$> readFile "token"
  --      messageOf result = FormattedMessage (EmojiIcon "one") "calcbot" channel [FormatAt user, FormatString ("Definition of \""<> text <> "\" is " <> " = " <> result)]
  --      resultText = define text
  --      debug = True
dict _ = return "Type more!"

main :: IO ()
main = do
    putStrLn ("+ Listening on port 8184")
    run 8184 (slashSimple dict)

{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, DeriveGeneric #-}

import BasePrelude hiding (intercalate, tail)
import Control.Lens ((^.), (^?))
import Data.Aeson.Lens (key, values)
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Text.Lazy hiding (span, drop, words, map, tail, filter)
import qualified Data.Text.Internal as TS
import Network.Linklater (say, slashSimple, Command(..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)
import System.Directory (doesFileExist)

data DictionaryResponse =
    DictionaryResponse { results :: Array
      } deriving (Show, Generic)

data WordResult =
    WordResult {  headword       :: Text
                , part_of_speech :: Text
                , senses         :: Array
                  } deriving (Show, Generic)

data Sense = 
    Sense { definition :: Text
      } deriving (Show, Generic)

instance FromJSON WordResult
instance ToJSON WordResult

instance FromJSON DictionaryResponse
instance ToJSON DictionaryResponse

instance FromJSON Sense
instance ToJSON Sense

apiKey :: IO (String)
apiKey = do
    keyExists <- doesFileExist "apiKey"
    case keyExists of
        True -> fmap (filter (/= '\n')) (readFile "apiKey")
        False -> return "lZTHxIMWBVZID7KsjnpM0Ds8wTbLveUg"

getWordResponses :: Text -> IO Array
getWordResponses word = do
    apiKeyText <- apiKey
    dictionaryResponse <- asJSON =<< get ("https://api.pearson.com/v2/dictionaries/entries?apikey=" <> apiKeyText <> "&search=english&headword=" <> unpack word) 
    return $ results $ dictionaryResponse ^. responseBody 

getFirstWordResponse :: Array -> Text -> Maybe WordResult
getFirstWordResponse arr word = 
  case (filteredResults V.!? 0) of
    (Just maybeResult) -> maybeResult
    (Nothing) -> Nothing
  where 
    parsedResults = V.map (\v -> parseMaybe parseJSON v) arr
    filteredResults = V.filter (hasSenses word) parsedResults

hasSenses :: Text -> Maybe WordResult -> Bool
hasSenses word mwr =
  case mwr of
    (Just wr) -> ((>) (V.length $ senses wr) 0) && (isJust (getFirstDefinitionSense (senses wr))) && ((headword wr) == word)
    (Nothing) -> False

getFirstDefinitionSense :: Array -> Maybe Sense
getFirstDefinitionSense s = 
  case (filteredResults V.!? 0) of
    (Just sense) -> sense
    (Nothing) -> Nothing
  where
    parsedResults = V.map (\v -> parseMaybe parseJSON v) s
    filteredResults = V.filter isJust parsedResults

getFirstSense :: Array -> Sense
getFirstSense arr = case parseFirstSense arr of
  (Just a) -> a
  (Nothing) -> getFirstSense $ V.tail arr
  where parseFirstSense arr' = parseMaybe parseJSON $ arr' V.! 0

define :: Text -> IO Text
define text = do
  w <- parsedWordResponses
  case getFirstWordResponse w text of
    (Just aWord) -> return ((headword aWord) <> " - " <> (part_of_speech aWord) <> ". " <> (definition . getFirstSense . senses $ aWord))
    (Nothing) -> return "No definition found :("
  where 
      parsedWordResponses = getWordResponses text

getUrban :: Text -> IO (Maybe Value)
getUrban text = do
  r <- get ("http://api.urbandictionary.com/v0/define?term=" <> unpack text)
  return $ r ^? responseBody . key "list" . values . key "definition"

urban :: Text -> IO Text
urban text = do
  urbanText <- getUrban text
  case urbanText of
    (Just (String value)) -> return (text <> " - " <> fromStrict value)
    _ -> return (pack $ show urbanText)

getCommandType :: Maybe Text -> (Text -> IO Text)
getCommandType (Just command) =
  case unpack command of
    "urban" -> urban
    _ -> define

dict :: Maybe Command -> IO Text
dict (Just (Command commandText user channel (Just text))) =
  getCommandType commandText text

dict _ = return "Type more!"

main :: IO ()
main = do
    putStrLn ("+ Listening on port 8184")
    run 8184 (slashSimple dict)

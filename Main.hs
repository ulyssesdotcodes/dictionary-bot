{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, DeriveGeneric #-}

import BasePrelude hiding (intercalate, filter, tail)
import Control.Lens ((^.), (^?))
import Control.Lens.Getter (Getting)
import Data.Aeson ((.:), (.:?), decode, encode, FromJSON(..), ToJSON(..), Value(..), fromJSON)
import Data.Aeson.Lens (key, values)
import Data.Aeson.Types
import Data.Attoparsec.Text.Lazy
import Data.Char (isLetter, isAscii)
import Data.Map (Map)
import Data.Vector as V
import Data.Text.Lazy hiding (span, drop, words, map, tail)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Internal as TS
import Data.ByteString.Lazy.Internal (ByteString)
import GHC.Generics
import Network.Linklater (say, slashSimple, Command(..), Config(..), Message(..), Icon(..), Format(..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)

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

getWordResponses :: Text -> IO Array
getWordResponses word = do
    dictionaryResponse <- asJSON =<< get ("https://api.pearson.com/v2/dictionaries/entries?apikey=lZTHxIMWBVZID7KsjnpM0Ds8wTbLveUg&search=english&headword=" <> unpack word) 
    return $ results $ dictionaryResponse ^. responseBody 

getFirstWordResponse :: Array -> Maybe WordResult
getFirstWordResponse arr = 
  case (filteredResults !? 0) of
    (Just maybeResult) -> maybeResult
    (Nothing) -> Nothing
  where 
    parsedResults = V.map (\v -> parseMaybe parseJSON v) arr
    filteredResults = V.filter hasSenses parsedResults

hasSenses :: Maybe WordResult -> Bool
hasSenses mwr =
  case mwr of
    (Just wr) -> ((>) (V.length $ senses wr) 0) && (isJust (getFirstDefinitionSense (senses wr)))
    (Nothing) -> False

getFirstDefinitionSense :: Array -> Maybe Sense
getFirstDefinitionSense s = 
  case (filteredResults !? 0) of
    (Just sense) -> sense
    (Nothing) -> Nothing
  where
    parsedResults = V.map (\v -> parseMaybe parseJSON v) s
    filteredResults = V.filter isJust parsedResults

getFirstSense :: Array -> Sense
getFirstSense arr = case parseFirstSense arr of
  (Just a) -> a
  (Nothing) -> getFirstSense $ tail arr
  where parseFirstSense arr' = parseMaybe parseJSON $ arr' ! 0

dict :: Maybe Command -> IO Text
dict (Just (Command user channel (Just text))) = do
  w <- parsedWordResponses
  case getFirstWordResponse w of
    (Just aWord) -> return ((headword aWord) <> " - " <> (part_of_speech aWord) <> ". " <> (definition . getFirstSense . senses $ aWord))
    (Nothing) -> return "No definition found :("
  where 
      parsedWordResponses = getWordResponses text

dict _ = return "Type more!"

main :: IO ()
main = do
    putStrLn ("+ Listening on port 8184")
    run 8184 (slashSimple dict)

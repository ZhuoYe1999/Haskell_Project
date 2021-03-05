{- |
   JSONParser		Group:16
-}

{-# LANGUAGE OverloadedStrings #-}

module JSONParser
    ( parseMovies
    , parseActors
    , parsePages
    , parseMovies2
     ) where

import DataStructures

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Maybe


-- ################################## Results #################################

data Results = Results {id :: Int, title :: String, releaseDate :: String}
  deriving (Show)

instance FromJSON Results where
   parseJSON (Object v) = Results <$> v .: "id" <*> v .: "title" <*> v .: "release_date"
   parseJSON _ = mzero

-- ################################## Results ##################################

data MovieFromJSON = MovieFromJSON {results :: [Results], pages :: Int}
  deriving (Show)

instance FromJSON MovieFromJSON where
   parseJSON (Object v) = MovieFromJSON <$> v .: "results" <*> v .: "total_pages"
   parseJSON _ = mzero

{- | This function parses a given byte string representing a JSON into the total
     number of pages -}
parsePages :: B.ByteString -> Int
parsePages b = fromMaybe 1 (parseMaybe pageParser =<< decode b)

pageParser :: Value -> Parser Int
pageParser = withObject "pageParser" $ \o -> o.: "total_pages"

-- ################################## Movie ####################################
instance FromJSON Movie where
   parseJSON (Object o) = Movie <$> o .: "id" <*> o .: "title" <*> o .: "release_date"
   parseJSON _ = mzero

-- | This function parses a given byte string representing a JSON into a list of movies
parseMovies :: B.ByteString -> [Movie]
parseMovies b = fromMaybe [] (parseMaybe movieParser =<< decode b)

movieParser :: Value -> Parser [Movie]
movieParser = withObject "movieParser" $ \o -> o.: "results"

-- ############################### Actors ######################################

data TmpActor = TmpActor Int String

instance FromJSON TmpActor where
    parseJSON (Object o) = TmpActor <$> o .: "id" <*> o .: "name"
    parseJSON _ = mzero

{- | This function parses a given byte string representing a JSON and a movie into a list
     actors -}
parseActors :: B.ByteString -> Movie -> [Actor]
parseActors cn movie = map (converteTmp movie) (fromMaybe [] (parseMaybe actorParser =<< decode cn))
  where
    converteTmp :: Movie -> TmpActor -> Actor
    converteTmp movie (TmpActor aId name) = Actor aId name [movie]

actorParser :: Value -> Parser [TmpActor]
actorParser = withObject "actorParser" $ \o -> o.: "cast"

-- ################################# Movie2 ####################################
instance FromJSON Movie2 where
  parseJSON (Object o) = Movie2 <$>  o .: "title"
  parseJSON _ = mzero

-- | This function parses a given byte string representing a JSON into a list of movies2
parseMovies2 :: B.ByteString -> [Movie2]
parseMovies2 m = fromMaybe [] (parseMaybe movies2Parser =<< decode m)

movies2Parser :: Value -> Parser [Movie2]
movies2Parser = withObject "movies2Parser" $ \o -> o.: "listings"


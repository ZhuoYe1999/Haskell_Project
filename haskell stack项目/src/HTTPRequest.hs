{- |
   HTTPRequest		Group:16
  
   To make HTTP requests and download data
-}
module HTTPRequest
    ( httpGetListOfMovies,
      httpGetListOfActores,
      getDateString
    ) where

import Data.List
import Control.Monad
import DataStructures
import JSONParser
import qualified Network.HTTP.Conduit as N
import Data.DateTime
import qualified Data.ByteString.Lazy as B
import Control.Exception

{- | For a given DateTime value from the "Data.DateTime" library a time string in
     yyyy-mm-dd format will be returned -}
getDateString :: DateTime  -> String
getDateString date = case toGregorian date of
  (yr, mth, day, _, _, _) ->  dShow yr ++ "-" ++ dShow mth ++ "-" ++ dShow day
    where
      dShow x = if x < 10 then "0" ++ show x else show x

{- | Looks up all movies starting from a given time string in format yyyy-mm-dd
at the "TheMovieDB" API. -}
httpGetListOfMovies :: String -> IO [Movie]
httpGetListOfMovies fromDate = do
  let movieHandle = (\e -> return B.empty) :: N.HttpException -> IO B.ByteString
  pages <- fmap parsePages (handle movieHandle (N.simpleHttp =<< movieReqURL fromDate 1))
  requestList <- mapM (N.simpleHttp <=< movieReqURL fromDate) [1..pages]
  return $ concatMap parseMovies requestList

-- produces the URL for the API call from a given date and a page number.
movieReqURL :: String -> Int -> IO String
movieReqURL fromDate i = do
  date <- getCurrentTime
  return $ concat ["https://api.themoviedb.org/3/discover/movie?api_key=",
                   "77a5749742a2117c0b9c739d7bad6518&language=en-US&sort_by=",
                   "popularity.desc&include_adult=false&include_video=false&page=",
                   show i, "&region=US&primary_release_date.gte=", fromDate,
                   "&primary_release_date.lte=", getDateString date]
    where dShow x = if x < 10 then "0" ++ show x else show x

-- ############################### Actors #####################################

-- | Looks up all actors playing in the given list of movies at the "TheMovieDB" API.
httpGetListOfActores :: [Movie] -> IO [Actor]
httpGetListOfActores movies = do
  actores <- mapM getActores movies
  return $ concatActors actores

-- executes the API request for a given movie.
getActores :: Movie -> IO [Actor]
getActores m@(Movie movieId _ _) = do
  let actorHandle = (\e -> return B.empty) :: N.HttpException -> IO B.ByteString
  actoreJSON <- handle actorHandle (N.simpleHttp (actorReqUrl movieId))
  return $ parseActors actoreJSON m

-- for a given movie ID the request URL for the movie lookup will be produced.
actorReqUrl :: Int -> String
actorReqUrl aId = concat [ "https://api.themoviedb.org/3/movie/", show aId,
                          "/credits?api_key=77a5749742a2117c0b9c739d7bad6518" ]


{- helper function that concatenates the actors so that duplicates with different
   movies are combined together -}
concatActors :: [[Actor]] -> [Actor]
concatActors x = removeDups $ concat x
  where
    removeDups :: [Actor] -> [Actor]
    removeDups []     = []
    removeDups (x:xs) = let dups = getDups x (x:xs) in concatA (fst dups) : removeDups (snd dups)

    getDups :: Actor -> [Actor] -> ([Actor], [Actor])
    getDups _ []                                       = ([], [])
    getDups a1@(Actor _ n1 _) (a2@(Actor _ n2 _) : xs) = if n1 == n2
                                                          then (a2 : fst (getDups a1 xs), snd $ getDups a1 xs)
                                                          else (fst $ getDups a1 xs, a2 : snd (getDups a1 xs))
    concatA :: [Actor] -> Actor
    concatA [a]                                         = a
    concatA (Actor aId1 n1 m1 : Actor aId2 n2 m2 : xs ) = concatA (Actor aId1 n1 (m1 ++ m2) : xs)

{- |
   Main		Group 16

   This purpose of the application is to download all the recent movies and stores them into a SQLite database.
   Main Functionality is as Follows: The user types in an actor in the Console; 
   The application will look this actor up and print out all movies he's playing in.
-}

module Main where

import HTTPRequest
import DataBase
import IO
import DataStructures
import JSONParser
import Data.Maybe
import Control.Exception
import Control.Monad
import qualified Network.HTTP.Conduit as N
import Data.DateTime

-- The main function of this applicaton
run :: IO ()
run = do
  conn <- dbConnect
  initialiseDB conn
  date <- getCurrentTime

  let lastMonthDay = getDateString (addMinutes (-2 * 30 * 24 * 60) date) -- two months
  cleanupDatabase conn (getDateString (addMinutes (-6 * 30 * 24 * 60) date)) -- six months
  lastMovieDate <- getDateOfLastMoveInDB conn
  let movieHandle = (\e -> return []) :: N.HttpException -> IO [Movie]
  listOfMovies <- handle movieHandle
                  (httpGetListOfMovies $ fromMaybe lastMonthDay lastMovieDate)
  let actorHandle = (\e -> return []) :: N.HttpException -> IO [Actor]
  listOfActors <- handle actorHandle (httpGetListOfActores listOfMovies)
  insertMovieIntoDB conn listOfMovies
  insertActorIntoDB conn listOfActors
  movies <- getMoviesFromDatabase conn
  actoreName <- askForActor
  movie <- searchMoviesInDB conn actoreName
  case movie of
    Nothing -> do
      disconnectDB conn
      error "Could't find a movie for the given actor"
    Just x -> printMovies x
  disconnectDB conn

-- | The main function of this application
main :: IO ()
main = do
  let mainHandler = (print . takeWhile (/= '\n') . show) :: SomeException -> IO ()
  handle mainHandler run

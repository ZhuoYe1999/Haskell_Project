{- |
   Module     : Tests
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

Some tests to test the application

Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}

{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import DataBaseModule
import DataStructures
import JSONParserModule
import HTTPRequestModule
--import HTTPRequestModule2
import qualified Data.ByteString.Lazy as B

main :: IO Counts
main = do
  runTestTT dataBaseModuleTests
  runTestTT jsonParserModuleTests
  runTestTT httpModuleTests
  runTestTT httpModule2Tests

-- #############################################################################
-- ############################# Database Tests#################################
-- #############################################################################
dataBaseModuleTests :: Test
dataBaseModuleTests = TestList
  [ TestLabel "Testing if the result is nothing" dataBaseTest,
    TestLabel "Testing the database module" dataBaseTest2,
    TestLabel "More complex test of the database module" dataBaseTest3,
    TestLabel "Trying a inconsistent database" dataBaseTest4,
    TestLabel "Trying a inconsistent database" dataBaseTest5,
    TestLabel "Cleaning up database 1" dataBaseCleanupTest,
    TestLabel "Cleaning up database 2" dataBaseCleanupTest1,
    TestLabel "Cleaning up database 3" dataBaseCleanupTest2,
    TestLabel "Cleaning up database 4" dataBaseCleanupTest3,
    TestLabel "Cleaning up database 5" dataBaseCleanupTest4,
    TestLabel "Getting date of the newest movie" dataBaseDateTest1,
    TestLabel "Getting date of the newest movie" dataBaseDateTest2 ]

setupTest :: [Movie] -> [Actor] -> String -> String -> Maybe[Movie] -> Test
setupTest movie actor name description expected = TestCase ( do
  conn <- dbConnect
  initialiseDB conn
  insertMovieIntoDB conn movie
  insertActorIntoDB conn actor
  movie <- searchMoviesInDB conn name
  clearDatabase conn
  disconnectDB conn
  assertEqual description expected movie
  )

dataBaseTest :: Test
dataBaseTest = setupTest [] [] "test" "Shouldn fine a movie" Nothing


dataBaseTest2 :: Test
dataBaseTest2 = setupTest
 [ Movie 1 "Movie" "2017-07-30" ] [ Actor 1 "Johannes" [ Movie 1 "Movie" "2017-07-30" ] ]
 "Johannes" "Should find a movie" (Just [ Movie 1 "Movie" "2017-07-30" ])

dataBaseTest3 :: Test
dataBaseTest3 = setupTest
  [ Movie 1 "Doctor Who" "2017-07-30", Movie 2 "Lord of the Rings" "2017-07-30",
    Movie 3 "Star Wars" "2017-07-30" ]
  [ Actor 1 "Johannes" [ Movie 1 "Doctor Who" "2017-07-30",
                         Movie 2 "Lord of the Rings" "2017-07-30",
                         Movie 3 "Star Wars" "2017-07-30" ],

    Actor 2 "Manuel" [ Movie 1 "Doctor Who" "2017-07-30"],
    Actor 3 "Liam" [] ]
  "Johannes" "Should find tree a movies"
  (Just [ Movie 1 "Doctor Who" "2017-07-30", Movie 2 "Lord of the Rings" "2017-07-30",
          Movie 3 "Star Wars" "2017-07-30" ])

dataBaseTest4 :: Test
dataBaseTest4 = setupTest
  [] [ Actor 1 "Clara Oswald" [ Movie 1 "Doctor Who" "2017-07-30" ]]
  "Clara Oswald" "Should return Nothig" Nothing

dataBaseTest5 :: Test
dataBaseTest5 = setupTest
  [ Movie 1 "Doctor Who" "2017-07-30" ] []
  "Clara Oswald" "Should return Nothig" Nothing

setupCleanupTest :: [Movie] -> String -> [Movie] -> String -> Test
setupCleanupTest movies date expected description = TestCase ( do
  conn1 <- dbConnect
  initialiseDB conn1
  insertMovieIntoDB conn1 movies
  cleanupDatabase conn1 date
  movies <- getMoviesFromDatabase conn1
  clearDatabase conn1
  disconnectDB conn1
  assertEqual description expected movies
  )

dataBaseCleanupTest :: Test
dataBaseCleanupTest = setupCleanupTest
  [ Movie 1 "TestMovie" "2016-01-01" ] "2017-01-01" []
  "Should not find any movies anymore"

dataBaseCleanupTest1 :: Test
dataBaseCleanupTest1 = setupCleanupTest
  [ Movie 1 "TestMovie" "2016-01-01", Movie 2 "TestMovie" "2017-01-02" ]
  "2017-01-01" [ Movie 2 "TestMovie" "2017-01-02" ] "Should find only movie 2"

dataBaseCleanupTest2 :: Test
dataBaseCleanupTest2 = setupCleanupTest [] "2017-01-01" [] "Should find any movie"

setupCleanupTest2 :: [Movie] -> [Actor] -> String -> String -> [Actor] -> Test
setupCleanupTest2 movies actores date description expected = TestCase ( do
  conn <- dbConnect
  initialiseDB conn
  insertMovieIntoDB conn movies
  insertActorIntoDB conn actores
  cleanupDatabase conn date
  actores <- getActorsFromDatabase conn
  clearDatabase conn
  disconnectDB conn
  assertEqual description actores expected
  )

dataBaseCleanupTest3 :: Test
dataBaseCleanupTest3 = setupCleanupTest2
  [ Movie 1 "Doctor Who" "2016-01-01", Movie 2 "Boradchurch" "2017-01-02"]
  [ Actor 1 "David Tennant" [ Movie 1 "Doctor Who" "2016-01-01",
                              Movie 2 "Boradchurch" "2017-01-02" ]]
  "2017-01-03" "The actore should be removed" []

dataBaseCleanupTest4 :: Test
dataBaseCleanupTest4 = setupCleanupTest2
  [ Movie 1 "Doctor Who" "2016-01-01", Movie 2 "Boradchurch" "2017-01-02" ]
  [ Actor 1 "David Tennant" [ Movie 1 "Doctor Who" "2016-01-01",
                              Movie 2 "Boradchurch" "2017-01-02" ]]
  "2016-02-01" "The movie schould be removed from the actore"
  [ Actor 1 "David Tennant" [ Movie 2 "Boradchurch" "2017-01-02" ]]

dataBaseDateTest1 :: Test
dataBaseDateTest1 = TestCase (do
  conn <- dbConnect
  initialiseDB conn
  insertMovieIntoDB conn [ Movie 1 "Doctor Who" "2016-03-01",
      Movie 2 "Boradchurch" "2017-01-02" ]
  insertActorIntoDB conn [ Actor 1 "David Tennant" [ Movie 1 "Doctor Who" "2016-01-01"]]
  date1 <- getDateOfLastMoveInDB conn
  clearDatabase conn
  disconnectDB conn
  assertEqual "ecpect the date of movie 1" date1 (Just "2017-01-02")
  )

dataBaseDateTest2 :: Test
dataBaseDateTest2 = TestCase (do
  conn <- dbConnect
  initialiseDB conn
  insertMovieIntoDB conn []
  insertActorIntoDB conn []
  date2 <- getDateOfLastMoveInDB conn
  clearDatabase conn
  disconnectDB conn
  assertEqual "ecpect the date of movie 1" date2 Nothing
  )

-- #############################################################################
-- ############################ JSON Parser Tests###############################
-- #############################################################################
jsonParserModuleTests :: Test
jsonParserModuleTests = TestList
  [ TestLabel "Should parse 2 movies" jsonMovieParserTest1,
    TestLabel "Should return a empty list" jsonMovieParserTest2,
    TestLabel "Should return a empty list" jsonMovieParserTest3,
    TestLabel "Should parse 1 movies" jsonMovieParserTest4,
    TestLabel "Should parse pages correctly" jsonPagesParserTest1,
    TestLabel "Should get an error and return default value" jsonPagesParserTest2,
    TestLabel "Should get an error and return default value" jsonPagesParserTest3,
    TestLabel "Should parse one actor" jsonActorParserTest1,
    TestLabel "Should get an error and return default value" jsonActorParserTest2,
    TestLabel "Should get an error and return default value" jsonActorParserTest3,
    TestLabel "Shoud parse one Movie2" jsonMovie2ParserTest1,
    TestLabel "Should get an error and return default value" jsonMovie2ParserTest2,
    TestLabel "Should get an error and return default value" jsonMovie2ParserTest3,
    TestLabel "Should parse one cinema" jsonCinemaParserTest1,
    TestLabel "Should get an error and return default value" jsonCinemaParserTest2,
    TestLabel "Should get an error and return default value" jsonCinemaParserTest3 ]

jsonParserTestGenerator :: (Eq a, Show a) => (B.ByteString -> a) -> B.ByteString -> String -> a -> Test
jsonParserTestGenerator parser json description expected = TestCase (do
 let result = parser json
 assertEqual description expected result
 )

-- ########################### Movie Parser Tests ##############################


movieJson :: B.ByteString
movieJson = "{\"page\":1,\"total_results\":127,\"total_pages\":7,\"results\": \
  \[{\"vote_count\":58,\"id\":371638,\"video\":false,\"vote_average\":7.7,\"title\": \
  \\"The Disaster Artist\",\"popularity\":204.431944,\"poster_path\":\"\\/uCH6FOFsDW6\
  \pfvbbmIIswuvuNtM.jpg\",\"original_language\":\"en\",\"original_title\":\
  \\"The Disaster Artist\",\"genre_ids\":[18,35,36],\"backdrop_path\":\"\\\
  \/bAI7aPHQcvSZXvt7L11kMJdS0Gm.jpg\",\"adult\":false,\"overview\":\"Some description \
  \\",\"release_date\":\"2017-12-01\"},{\"vote_count\":14,\"id\":429189,\"video\": \
  \false,\"vote_average\":3.3,\"title\":\"Wonder Wheel\",\"popularity\":108.762089,\
  \\"poster_path\":\"\\/fPXn8SW4pa4kJErAIAJLmb3Znns.jpg\",\"original_language\":\"en\
  \\",\"original_title\":\"Wonder Wheel\",\"genre_ids\":[18],\"backdrop_path\":\"\\/\
  \jGYeZzcAG0df2nWRLJW2CiweyyG.jpg\",\"adult\":false,\"overview\":\"some description\
  \.\",\"release_date\":\"2017-12-01\"}]}"

jsonMovieParserTest1 :: Test
jsonMovieParserTest1 = jsonParserTestGenerator parseMovies  movieJson
                          "Schould find 2 movies"
                          [ Movie 371638 "The Disaster Artist" "2017-12-01",
                            Movie 429189 "Wonder Wheel" "2017-12-01"]

jsonMovieParserTest2 :: Test
jsonMovieParserTest2 = jsonParserTestGenerator parseMovies "Invalid text"
                          "Shouldn't find an movie" []

jsonMovieParserTest3 :: Test
jsonMovieParserTest3 = jsonParserTestGenerator parseMovies
                          "{\"page\":1, \"total_pages\":7,\
                          \ \"results\": [{\"id\":\"371638\", \"title\":\"Test\", \
                          \\"release_date\":\"2017-12-01\" }]}"
                          "Schould get parse error and return empty list" []

jsonMovieParserTest4 :: Test
jsonMovieParserTest4 = jsonParserTestGenerator parseMovies
                          "{\"page\":1, \"total_pages\":7,\
                          \ \"results\": [{\"id\":371638, \"title\":\"Test\", \
                          \\"release_date\":\"2017-12-01\" }]}"
                          "Schould fine one movie"
                          [Movie 371638 "Test" "2017-12-01"]

-- ########################### Parge Parser Tests ##############################
jsonPagesParserTest1 :: Test
jsonPagesParserTest1 = jsonParserTestGenerator parsePages
                          "{\"page\":1, \"total_pages\":7,\
                          \ \"results\": [{\"id\":371638, \"title\":\"Test\", \
                          \\"release_date\":\"2017-12-01\" }]}"
                          "Schould return seven" 7

jsonPagesParserTest2 :: Test
jsonPagesParserTest2 = jsonParserTestGenerator parsePages
                          "{\"page\":1, \"total_pages\":\"7\",\
                          \ \"results\": [{\"id\":371638, \"title\":\"Test\", \
                          \\"release_date\":\"2017-12-01\" }]}"
                          "Schould get an error and return 1" 1

jsonPagesParserTest3 :: Test
jsonPagesParserTest3 = jsonParserTestGenerator parsePages
                          "Invalid Input"
                          "Schould get an error and return 1" 1

-- ########################### Actor Parser Tests ##############################
jsonActorParserTest1 :: Test
jsonActorParserTest1 = jsonParserTestGenerator (`parseActors` Movie 1 "Test" "2017-12-01")
                         "{\"id\":123,\"cast\":[{\"id\":16270,\"name\":\"Johannes\"}]}"
                         "Should parse one Actor"
                         [Actor 16270 "Johannes" [Movie 1 "Test" "2017-12-01"]]

jsonActorParserTest2 :: Test
jsonActorParserTest2 = jsonParserTestGenerator (`parseActors` Movie 1 "Test" "2017-12-01")
                        "Invalid Input" "Should get an error and return an empty list" []

jsonActorParserTest3 :: Test
jsonActorParserTest3 = jsonParserTestGenerator (`parseActors` Movie 1 "Test" "2017-12-01")
                         "{\"id\":\"123\",\"cast\":[{\"id\":16270,\"name\":Johannes}]}"
                         "Should get an error and return an empty list" []

-- ########################### Movie2 Parser Tests ##############################
jsonMovie2ParserTest1 :: Test
jsonMovie2ParserTest1 = jsonParserTestGenerator parseMovies2
                          "{\"status\":\"ok\",\"listings\":[{\"title\":\"TestMovie\"\
                          \,\"times\":[\"13:40\",\"19:30\"]}]}"
                          "Schould parse one Movie2" [Movie2 "TestMovie"]

jsonMovie2ParserTest2 :: Test
jsonMovie2ParserTest2 = jsonParserTestGenerator parseMovies2
                          "Invalid Input"
                          "Shouldn't parse any movie" []

jsonMovie2ParserTest3 :: Test
jsonMovie2ParserTest3 = jsonParserTestGenerator parseMovies2
                          "{\"status\":\"ok\",\"listings\":[{\"title\":TestMovie\
                          \,\"times\":[\"13:40\",\"19:30\"]}]}"
                          "Should get an error and return a empty list as default value"
                          []

-- ########################### Cinema Parser Tests ##############################

jsonCinemaParserTest1 :: Test
jsonCinemaParserTest1 = jsonParserTestGenerator parseCinemas
                          "{\"postcode\":\"E13DH\",\"cinemas\":[{\"name\":\
                          \\"TestCinema\",\"id\":\"10480\",\"distance\":1.33}]}"
                          "Should return one cinema" [Cinema "10480" "TestCinema" 1.33]

jsonCinemaParserTest2 :: Test
jsonCinemaParserTest2 = jsonParserTestGenerator parseCinemas
                          "{\"postcode\":\"E13DH\",\"cinemas\":[{\"name\":\
                          \\"TestCinema\",\"id\":10480,\"distance\":1.33}]}"
                          "Should get an error and return a empty list as default value"
                          []

jsonCinemaParserTest3 :: Test
jsonCinemaParserTest3 = jsonParserTestGenerator parseCinemas
                          "Invalid Input"
                          "Should get an error and return a empty list as default value"
                          []

-- #############################################################################
-- ############################ HttpModule2 Tests###############################
-- #############################################################################
httpModuleTests :: Test
httpModuleTests = TestList
  [ TestLabel "Should return an empty list" httpModuleActorListTest1,
    TestLabel "Should return an empty list" httpModuleActorListTest2 ]

httpModuleActorListTest1 :: Test
httpModuleActorListTest1 = TestCase (do
  actoreList <- httpGetListOfActores []
  assertEqual "Should be emptylist" [] actoreList
 )

httpModuleActorListTest2 :: Test
httpModuleActorListTest2 = TestCase (do
  actoreList <- httpGetListOfActores [Movie 1 "TestMovie" "2017-12-12"]
  assertEqual "Should be emptylist" [] actoreList
 )

{- this test actually works but has to much actors to it would be to long
httpModuleActorListTest3 :: Test
httpModuleActorListTest3 = TestCase (do
  actoreList <- httpGetListOfActores [Movie 371638 "The Disaster Artist" "2017-12-01"]
  assertEqual "Should return all actores of The disaster Artist"
   [ Bunch of actores]
   actoreList
 )-}
-- #############################################################################
-- ############################ HttpModule2 Tests###############################
-- #############################################################################
httpModule2Tests :: Test
httpModule2Tests = TestList
  [ TestLabel "Should return all cineams in stratford" httpModule2CinemaListTest1,
    TestLabel "Should return an empty list" httpModule2CinemaListTest2,
    TestLabel "Should return the given cinema" httpModule2CinemaListTest3,
    TestLabel "Should return an empty list" httpModule2CinemaListTest4]

httpModule2CinemaListTest1 :: Test
httpModule2CinemaListTest1 = TestCase (do
  cinemaList <- httpGetCinemaList "Stratford"
  assertEqual "Should return all cineams in stratford"
    [ Cinema "6994" "Stratford Picturehouse, London" 0.25,
      Cinema "10634" "Stratford-upon-Avon Picturehou Picturehouse, CV37 6NL" 0.32,
      Cinema "9472" "VUE Leamington Spa, Leamington Spa" 9.67,
      Cinema "9474" "VUE Redditch, Redditch" 12.81,
      Cinema "7542" "Cineworld Solihull, Solihull" 15.66,
      Cinema "9408" "Odeon Coventry, "17.19,
      Cinema "9448" "Odeon Banbury, Banbury" 17.83,
      Cinema "10146" "Cineworld Birmingham - NEC, Bickenhill" 18.42,
      Cinema "7125" "Empire Birmingham Great Park, Birmingham" 19.18] cinemaList
  )

httpModule2CinemaListTest2 :: Test
httpModule2CinemaListTest2 = TestCase (do
  cinemaList <- httpApiCinemaRequest (Movie 1 "TestMove" "2017-12-12")
    [ Cinema "6994" "Stratford Picturehouse, London" 0.25 ]
  assertEqual "Should return an empty list" [] cinemaList
  )

httpModule2CinemaListTest3 :: Test
httpModule2CinemaListTest3 = TestCase (do
  cinemaList <- httpApiCinemaRequest (Movie 1 "Paddington 2" "2017-12-12")
    [ Cinema "6994" "Stratford Picturehouse, London" 0.25 ]
  assertEqual "Should returnthe given cinema"
    [ Cinema "6994" "Stratford Picturehouse, London" 0.25 ] cinemaList
  )

httpModule2CinemaListTest4 :: Test
httpModule2CinemaListTest4 = TestCase (do
  cinemaList <- httpApiCinemaRequest (Movie 1 "Paddington 2" "2017-12-12") []
  assertEqual "Should return an empty list" [] cinemaList
  )

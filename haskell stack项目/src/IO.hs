{- |
   IO		Group:16
  
   The IO moudle of this app

-}

module IO
    ( 
      askForActor,
      printMovies,
    ) where

import DataStructures


{- | This function asks the user for an actor and returns it as a string -}
askForActor :: IO String
askForActor = do
  putStrLn "Please enter the actor name you want to search for: "
  getLine

{- | This function prints a given list of movies to std out -}
printMovies :: [Movie] -> IO()
printMovies x = do
  putStrLn "The given actor plays in the following movies"
  printMoviesHelper x 1
    where
      printMoviesHelper :: [Movie] -> Int -> IO()
      printMoviesHelper [] _ = return ()
      printMoviesHelper (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show x)
        printMoviesHelper xs (i + 1)

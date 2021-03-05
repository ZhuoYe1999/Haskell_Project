{- |
   DataStructures		Group:16

   Datatypes of haskell.

-}
module DataStructures(
      Movie(Movie),
      Actor(Actor),
      Movie2(Movie2)
    ) where

{- | Data structure representing a movie. The first parameter is the ID and the
     second is the name of the movie and the third one is the release day -}
data Movie = Movie { movieId :: Int, title :: String, releaseDate :: String }
  deriving (Eq)

instance Show Movie where
  show (Movie _ title _) = title

{- | Data structure representing a Movie from the second API, whicht have no cinemaId and
     release date. -}
newtype Movie2 = Movie2 { movieTitle :: String }
  deriving (Eq, Show)

{- | Data structure representing an actor. The first parameter is the ID, the
     second is the name of the actor and the third is a List containing all movies he plays in -}
     -- maybe use list of movies
data Actor = Actor { actorId :: Int, actorName :: String, movie :: [Movie]}
  deriving (Eq, Show)


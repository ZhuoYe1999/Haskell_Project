# QMUL-FunctionalProgrammingGroupProject
This application is downloading all recent movies from the TheMovieDB api and
stores it into a SQLite database. The user types in an actor and this application
is looking this actor up and print out all movies he's playing in. For a given
location it suggests cinemas in the areas that play this movie.

Only movies that are not already in the database will be downloaded and stored to
save time.

## Example Input

> Please enter the actore name you want to search for:   
> Ben Affleck    
> The given actor plays in the following movies    
> (1) Justice League    
> Please selecte a movie now:    
> 1    
> Please enter your location:    
> Stratford    
> The following cinemas in your area show this film:    
> Cinema: "Stratford Picturehouse, London" Distance: 0.25    
> Cinema: "VUE Leamington Spa, Leamington Spa" Distance: 9.67    
> Cinema: "VUE Redditch, Redditch" Distance: 12.81   
> Cinema: "Cineworld Solihull, Solihull" Distance: 15.66    
> Cinema: "Odeon Coventry, " Distance: 17.19    
> Cinema: "Cineworld Birmingham - NEC, Bickenhill" Distance: 18.42   
> Cinema: "Empire Birmingham Great Park, Birmingham" Distance: 19.18    

## Build an execute
To build this project just execute in the main folder:
`stack build`
To execute:
`stack exec groupproject-exe`

Note: If you run the programm for the first type, it may take a while. All movies
have to be downloaded and stored into the database.

## Tests
This application provides a set of tests. To run the tests execute `stack test`
in the main folder.

Note: The tests are cleaning the database. If you run the program again, every
movie have to be downloaded again.

## Database cleanup
By default only the movies of the last thwo months will be downloaded. Further all movies
older than six months will be deleted from the database to save disk space.

## Team
The team for this project consists of thre master students at Queen Mary University London:   
Johannes Hartmann [Github Profile](https://github.com/IncredibleHannes)   
Liam Kelly [Github Profile](https://github.com/liamsscreenname)   
Manuel Campos Villarreal [Github Profile](https://github.com/cvmanuel)


## Haddoc
The haddoc documentation can be found at:
https://incrediblehannes.github.io/QMUL-FunctionalProgrammingGroupProject/index.html

## Used API's
[1] https://www.themoviedb.org/documentation/api

[2] https://api.cinelist.co.uk/

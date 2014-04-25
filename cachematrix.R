## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## args: matrix --assumption:invertible--
## procedure: creates a list including:
##            a setter - nullifies return of solve and set matrix input
##            a getter - retrieves the matrix input
##            a setter for solver cache - defines the function solve and returns 
##                                         to results variable
##           a getter for solver cache - reads the return value for the function 
##                                       in cache

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(X){
        x<<-X
        s<<-NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
##args: matrix --assumption:invertible--, ... for composing solve function
## procedure: 
##           retrieves caches value of getsolver
##           if value != retrieve cache with message
##           else, calculates invert, cache it and returns it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          s <- x$getsolve()
          if(!is.null(s)) {
            message("getting cached data")
            return(s)
          }
          data <- x$get()
          s <- solve(data, ...)
          x$setsolve(s)
          s    
}

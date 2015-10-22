## Put comments here that give an overall description of what your
## functions do

## here the function mackeCacheMatrix, basically store the inversed matrix version
## to x
makeCacheMatrix <- function(x = matrix()) {
        inversedM <- NULL
        
        set <- function(y) {
                x <<- y
                inversedM <<- NULL
        }
        
        get <- function() x
        
        setInversed <- function(inversedM) inversedM <<- inversedM
        
        getInversed <- function() inversedM
        
        list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## basically what it does here, is check if the x inversed matrix version is null
## if it is null, it creates through the solve function, a new matrix inversed and 
## then set it to x inversed variable, if not, it just returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverserd <- x$getInversed()
        if (!is.null(inverserd)) {
                message("getting cached data")
                return(inverserd)
        }
        
        matrix <- x$get()
        
        inversed <- solve(matrix)
        
        x$setInversed(inversed)
        
        inversed
}

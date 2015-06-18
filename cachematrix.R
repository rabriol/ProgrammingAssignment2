## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

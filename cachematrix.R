## makeCacheMatrix function that takes a matrix (from argument M) and creates
## a special "matrix" object with a list of functions that can cache the inverse of the matrix.
## In addition the function is able to send the inverse value to the global environment
makeCacheMatrix <- function(M = matrix()) {
        i <- NULL # cleaning inverse value in local environment
        get <- function() M # function get, get the value of the matrix 'M'
        setinv <- function(inverse) i <<- inverse # function setinv, set the value of the inverse 'i' in global environment
        getinv <- function() i # function getinv, get the value of the inverse 'i'
        list(get = get, setinv = setinv, getinv = getinv) # creates a list with all the functions
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the function should retrieve
## the inverse from the cache (stored in the global environment), so the functionfirst checks to see if the inverse
## has already been calculated using this --> if(!is.null(i)). If TRUE skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache using the setmean() function.
cacheSolve <- function(M, ...) {
        i <- M$getinv()
        if(!is.null(i)) {
                message("getting cached data!!")
                return(i)
        }
        data <- M$get()
        cat("\ncalculating the inverse:\n")
        i <- solve(data, ...)
        M$setinv(i)
        i
}

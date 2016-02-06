## Programming Assignment 2: Lexical Scoping

This second programming assignment will require you to write an R
function that is able to cache potentially time-consuming computations.
For example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. In this
Programming Assignment you will take advantage of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

### examples for executing the functions:

<!-- -->
    ## example of invertible matrix
    MATRIX1 <- matrix(c(2, 1, 5, 3), ncol = 2) # creating a matrix example
    MATRIX2 <- matrix(c(1, 2, 4, 0, -1, 1, 2, 3, 8), ncol = 3) # creating a matrix example
    
    ## Using functions for MATRIX1
    MATRIX.cache <- makeCacheMatrix(MATRIX1) # storing list of funtions with the vector inside
    cacheSolve(MATRIX.cache) # calculating or retrieving vector mean
    
    ## Using functions for MATRIX2
    MATRIX.cache <- makeCacheMatrix(MATRIX2) # storing list of funtions with the vector inside
    cacheSolve(MATRIX.cache) # calculating or retrieving vector mean

### And the functions:
    ## makeCacheMatrix function that takes a matrix (from argument M) and creates
    ## a special "matrix" object with a list of functions that can cache the inverse of the matrix.
    ## In addition the function is able to send the inverse value to the global environment
    makeCacheMatrix <- function(M = matrix()) {
            i <- NULL # cleaning inverse value in local environment
            get <- function() M # function get, get the value of the matrix 'M'
            setinv <- function(inverse) i <<- inverse # set the value of the inverse 'i' in global environment
            getinv <- function() i # get the value of the inverse 'i'
            list(get = get, setinv = setinv, getinv = getinv) # creates a list with all the functions
    }
    
    ## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
    ## If the inverse has already been calculated (and the matrix has not changed), then the
    ## function should retrieve the inverse from the cache (stored in the global environment),
    ## so the functionfirst checks to see if the inverse
    ## has already been calculated using this --> if(!is.null(i)). If TRUE skips the computation.
    ## Otherwise, it calculates the inverse of the matrix and sets the value in the cache
    ## using the setmean() function.
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



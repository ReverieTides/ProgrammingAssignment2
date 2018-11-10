#This file contains two functions, one for storing data inside a function/cache
#whilst the other is for recalling that information

#makeCacheMatrix will create list of several functions which will allow the
#functions to 'carry around' data

makeCacheMatrix <- function(x = matrix()) {
        invmtrx <- NULL
        set <- function(y) {
            x <<- y
            invmtrx <<- NULL
        }
        get <- function() x
        setinv <- function(solve)invmtrx <<- solve
        getinv <- function() invmtrx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve will check to see if the calculation has been performed before
#and if so, it will recall the data from the cache as opposed to a new calculation
#if not, it will perform solve on x

cacheSolve <- function(x, ...) {
        invmtrx <- x$getinv()
        if(!is.null(invmtrx)) {
            return(invmtrx)
        }
        result <- x$get()
        invmtrx<-solve(result)
        x$setinv(invmtrx)
        invmtrx
    }
    

## Put comments here that give an overall description of what your
## functions do

## This function does the following: 
## creates a matrix and gets its value
## sets the value of the inverse and gets its value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinv <- function(inv) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getminv = getinv)
}


## This function does the following:
## initial check if the inverse of the matrix is already in the cache
## if so, returns its value
## else, calculates the mean and set the value to in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

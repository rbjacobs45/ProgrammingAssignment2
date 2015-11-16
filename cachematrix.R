#-------------------------------------------------------------------------------
# This pair of functions calculates the inverse of a square matrix and caches 
# the solution. When the inverse is needed and the matrix has not changed, 
# then the inverse is looked up in the cache. If the matrix has changed, a new 
# inverse is computed and stored in the cache.
#-------------------------------------------------------------------------------

# The makeCacheMatrix function takes a square matrix "x" as an argument and 
# returns a list of functions that can be used to set the matrix values,
# get the matrix values, set the inverse values, and get the inverse values
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # The set function can be used to store new matrix values in the cache, 
        # after makeCacheMatrix has been called at least once before. This 
        # function will set i to NULL in the cache so that the cacheSolve will
        # compute a new inverse and store it in the cache, instead of returning 
        # the old inverse from the cache.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


# The cacheSolve function takes a list as an argument that is output from the 
# makeCacheMatrix function. Then, it checks to see if the inverse solution 
# already exists in the cache. If the inverse solution already exists 
# (i != NULL) in the cache, the inverse values are returned. Otherwise 
# (i == NULL), the inverse is computed using the solve function, and the 
# solution is stored in the cache. Also, note that additional arguments (...) 
# can be passed to cacheSolve that are arguments for the solve function.
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        # If i is not NULL, then return the inverse from cache
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        # If i is NULL, get the matrix values, compute the inverse, 
        # cache the solution, and return the inverse.
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

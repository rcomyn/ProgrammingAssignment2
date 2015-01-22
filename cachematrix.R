# Matrix inversion is usually a costly computation and their may be some benefit
#   to caching the inverse of a matrix rather than compute it repeatedly.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<-solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

# This function computes the inverse of the special "matrix" 
#   returned by makeCacheMatrix above. If the inverse has already been
#   calculated (and the matrix has not changed), then the cachesolve 
#   Will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}

# Test
# m2 <- matrix(c(4,3,3,2), 2, 2)
# m2
# mc2 <- makeCacheMatrix(m2)
# cacheSolve(mc2)
# cacheSolve(mc2)

# m3 <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
# m3
# mc3 <- makeCacheMatrix(m3)
# cacheSolve(mc3)
# cacheSolve(mc3)

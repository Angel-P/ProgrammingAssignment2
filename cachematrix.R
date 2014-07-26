## function MakeCacheMatrix creates a matrix entity and loads it into the cache. 
## New Values can be pushed into the matrix by the function setinv
## cacheSolve returns the inverse of whatever matrix was loaded into it by
## function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## sets values inside matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x ## prints currently loaded matrix
        setinv <- function(inv) m <<- inv
        getinv <- function() m ## feeds matrix data to cacheSolve
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        m <- x$getinv() ## checks if data is already cached before calculating
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() ## loads data into function
        m <- solve(data) ## solves matrix
        x$setinv(m) 
        m
}
## A pair of functions that cache the inverse of a matrix

##creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If inverse has already been calculated, then cachesolve retrieves inverse from cache.
cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s ## Return a matrix that is the inverse of 'x'
}
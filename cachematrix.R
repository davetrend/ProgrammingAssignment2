## Programming Assignment 2
## dave treff   dave@davetreff.com
## 4/28/2017
## 
## These functions demonstrate data cacheing, in this case to optimize performance of the
## slow solve() function used for inverting matrices.

## Function makeCacheMatrix
## Takes a matrix parameter, builds a cache for its value.
## Returns a list of cache functions -- get/set value, getinverse/setinverse -- for
## treating the cache as an opaque object 

makeCacheMatrix <- function(x = matrix()) {

       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(s) m <<- s
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function cacheSolve
## Takes an assumed-loaded cacheMatrix function as parameter.
## Returns the inverse of the previously cached matrix.


cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m

}

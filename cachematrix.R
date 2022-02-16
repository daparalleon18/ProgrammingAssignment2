## Below are two functions that gets the inverse of a matrix 
# efficiently in terms of run time and computing consumption.

## The following function "makeCacheMatrix" contains a pair of getters and setters
# to store matrix and inverse on x and inv respectively. These will serve as the cache variables.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL 
        }

        get <- function() {
                x
        }

        setinverse <- function(inverse) {
                inv <<- inverse
        }

        getinverse <- function() {
                inv
        }

        list(set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
}


## The following function "cacheSolve" returns the inverse of input matrix.
# It first evaluates if desired output is stored in cache before performing
# "solve" function (if necessary) in order to minimize unnecessary computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}

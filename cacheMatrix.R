## The following functions cache the inverse of a given matrix

## The 'makeCacheMatrix' function creates a special matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solveMatrix) inverse <<- solveMatrix
        getinv <- function() inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' computes the inverse of the special matrix returned by the above
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inv)){
                message('getting cached data')
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinv(inverse)
        inverse
}

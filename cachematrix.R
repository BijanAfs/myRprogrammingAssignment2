## makeCacheMatrix creates a matrix object that stores its inverse.
## Components of the matrix object are:
##      set         : sets the matrix
##      get         : outouts the matrix
##      setinverse  : calculates and caches the inverse
##      getinverse  : outputs the inverse of the matrix
##  Example: initiate with g <- makeCacheMatrix()
##          set a value: g$set(matrix(1:4, 2, 2))

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(a){
        x <<- a
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve returns the inverse of the matrix created with makeCacheMatrix
##  If the inverse is not cached, the inverse is calculated anew.
##  If the inverse is cahced, the cached inverse is returned.
##  Example:
##      Initiate with g <- makeCacheMatrix()
##      Set a value: g$set(matrix(1:4, 2, 2))
##      Calculate inverse: cacheSolve(g)
cacheSolve <- function(x, ...) {

    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    
}

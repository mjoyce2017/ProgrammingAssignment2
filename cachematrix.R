## These two functions are made to be used in unison to prevent redundant 
## computing. They work by creating an essentially blank space that can store
## a matrix and its inverse. The second function looks into the first function
## to see if a matrix's inverse has already been computed before computing.
## the inverse itself. This process utilizes lexical scoping to raise computing
## efficiency and to save time.


## makeCacheMatrix creates a "matrix" by setting the value of the matrix, 
## getting the value of the matrix, setting the value of the inverse, and   
## getting the value of the inverse This is a space where both the matrix and
## its inverse can be stored.

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
      x <<- y
      a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the previously created "matrix".
## However, it first checks to see if the inverse has previously been calculated.
## If it has been calculated, the inverse is pulled from the cache and the
## redundant computation is skipped. If there has not been a previously found
## inverse, it will calculate the inverse and set the value in the cache.

cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if(!is.null(a)) {
      message("getting cached data")
      return(a)
    }
    matri <- x$get(0)
    a <- solve(matri, ...)
    x$setinverse(a)
    a
}

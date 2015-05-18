## With these pair of functions one can cache the inverse of a matrix to
## avoid computing the same thing again, if the matrix to be inversed has not changed


## 1) makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse
## It creates a special "vector", which is actually a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
## Make sure the matrix you submit to the function is ivertible! (Assumption!!)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 2) cacheSolve: 
## This function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data (using solve()) and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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


## Example matrix for testing
## remove "#" for testing
# X <- matrix(rnorm(9), 3)
# X
# mM <- makeCacheMatrix(X)
# cM <- cacheSolve(mM)




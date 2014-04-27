## Matrix Inverse Cache by Inder
## The R script check if an inverse of the matrix already exists in the cache
## If the inverse exists in cache, computation is not repeated
## If cache contains inverse of another matrix, the Matrix Inverse is computed
## Inverse is then moved to cache

## The first function, `makeCacheMatrix` creates a special "Matrix", which is
## really a list containing a function to

#1.  set the value of the Matrix
#2.  get the value of the Matrix
#3.  set the value of the Matrix Inverse (inv)
#4.  get the value of the Matrix Inverse (inv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the Matrix Inverse of the special "Matrix"
## created with the above function. However, it first checks to see if the
## Matrix Inverse has already been calculated. If so, it `get`s the Matrix Inverse from the
## cache and skips the computation. Otherwise, it calculates the Matrix Inverse of
## the data and sets the value of the Inverse in the cache via the `setInverse`
## function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ## Solve() returns the inverse of a Vector
  x$setInverse(inv)
  inv
}

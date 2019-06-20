## Cache the Inverse of a Matrix Jun 20 2019


## The calculation of the inverse of a matrix is usally very intensive and it depends on the matrix nrow and ncol.
## The function below calculate the inverse of a matrix and it also caches its inverse to avoid double or repeated 
## calculation

## This function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function aims to calculate the inverse of the matrix created with the function above . If the inverse has already been 
## computed and the matrix has not been change then the function  retrieve the value from cache otherwise the inerse is calculated 
## again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message(" cached inv matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

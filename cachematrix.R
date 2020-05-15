## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #The function depends on x class needs to be matrix
  inv <- NULL 
  set <- function(y){ 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix # Stablish the matrix to be solved
  getInverse <- function() inv # Solves the inverse matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #Creates a list with the inv matrixes

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() 
  if(!is.null(inv)){# Verifies that the matrix is not null
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      # Gets the cached inverse matrixes
}

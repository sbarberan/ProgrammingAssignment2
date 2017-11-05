## This function is to calculate the inverse of a matrix and to store that on cache so its available
## at the environment to minimize computing

## This is to create the special matrix
## use by makeCacheMatrix(matrix) -> special_matrix
## where matrix is the matrix that you want to invert

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This checks if the inverse matrix is already in cache, if its not it uses solve to compute the inverse
## To get the inverse matrix use cacheSolve(special_matrix)
## if the cache already contains the inverse then it prints getting cached data
## if it needs to compute the inverse for the first tiem it only returns the inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

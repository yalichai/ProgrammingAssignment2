## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_matrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. First checks to see if the inverse has already been calculated,
# then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}

#test makeCacheMatrix
inv_test <- makeCacheMatrix(matrix(c(1,-2,-1,3),2,2))
inv_test$get_matrix()

#test cacheSolve
inv_test$get_inverse()
cacheSolve(inv_test)
cacheSolve(inv_test)

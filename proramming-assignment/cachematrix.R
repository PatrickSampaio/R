## Put comments here that give an overall description of what your
## functions do

## Extension to the list type

makeCacheMatrix <- function(matrix_value = matrix()) {
  cached_matrix_value = NULL
  set <- function(new_marix){
    matrix_value = new_marix
    cached_matrix_value = NULL
  }
  get <- function() matrix_value
  setinverse <- function(inverse) cached_matrix_value <<- inverse
  getinverse <- function() cached_matrix_value
  list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Logic behind the caching of the inverse of the matrix

cacheSolve <- function(x, ...) {
       cached_inverse = x$getinverse()
       if(!is.null(cached_inverse)){
           message("Getting cached value") 
           return(cached_inverse)
       }
       data = x$get()
       cached_inverse = solve(data)
       x$setinverse(cached_inverse)
       return(cached_inverse)
}

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix with inverse = NULL, and stores 4 functions: get,set,getinverse, and setinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(new_matrix){
    x<<-new_matrix
    i<<-NULL
  }
  get<- function() x
  setinverse<- function(inverse) i<<- inverse
  getinverse<- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes the matrix in the function above and calculates its inverse if it is NULL,and otherwise returns the cached inverse

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix<- x$get()
  inverse<- solve(matrix)
  x$setinverse(inverse)
  inverse
}

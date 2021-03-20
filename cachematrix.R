
## The makeCacheMatrix function defines the functions of the matrix given. Also it sets
## the inverse of the matrix as NULL and returns the list with all the functions
## required to interact with the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <- y
    inverse <- NULL
  }
  get <- function() {x}
  setinverse <- function(i) {inverse <- i}
  getinverse <- function() {inverse}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function first try to get the inverse of x and if it hasn't been 
## computed, it computes it and sets it in the cache as the inverse matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}

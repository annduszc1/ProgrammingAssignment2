## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## first m is null, then if we use set, we're setting new metrix (the first one was x, now can be y) plus assigning null to m
## 'get' gives us the matrix that is now chosen
## set inverse is assigning values of inverted matrix (not solving)
## getinverse is showing these values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## first the function is assigning to m, the value of getinverse from the makeCacheMatrix funkction
## if is not null, it means that some value was set, so the function gives us the value set in previus function
## id was null, then we are taking the values of matrix and assigning to data
## and calculating m as inverse of this data
## and then setting inverse = assigning these values for the mekacacheMatrix function for setinverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##library(MASS) is used to calculate inverse for non square as well as square matrices.
library(MASS)

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL      #initializaing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x       #function to get matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function()inv
  getinv <- function(){
                       inver<-ginv(x)
                       inver%*%x    #function to obtain inverse of the matrix
                      }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...)      ##gets cache data
  {
  inv <- x$getinv()
  if(!is.null(inv)) {                     #checking whether inverse is Null
    message("getting cached data")
    return(inv)                           #returnsinverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)                     #calculate inverse value
  x$setinv(inv)
  inv                              ##Return a matrix that is the inverse of 'x'
}


## There are two functions makeCacheMatrix, cacheSolve
## makeCacheMatrix is used to calculate inverse for non squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL    #initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x     #function to get matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function() {
                        inver <-ginv(x)
                        inver%*%x
                        }  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This is used to get the cache data

cacheSolve <- function(x, ...){       ## get cache data
  inv <- x$getinv()
  if(!is.null(inv)){             ## checking if inverse is NULL
    message("getting cached data")
    return(inv)           ## return inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)       ## calculate inverse value
  x$setinv(inv)
  inv      ## return a matrix that is the inverse of 'x'
}

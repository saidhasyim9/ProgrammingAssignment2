## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function will return the list of 4 functions to be used for caching
makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  
  ##create a set function
  set <- function (y) {
    x <<- y
    invr <<- NULL
  }
  
  ##create a get function
  get <- function () x
  
  ##create a set function with value
  setInv <- function (invrValue) invr <<- invrValue
  
  ##create a get function when the value is available
  getInv <- function () invr
  
  ## return the list of 4 functions
  list (set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## Write a short comment describing this function
## this function is used to cache result generated previously
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr <- x$getInv()
    
    ## when there is existing data
    if (!is.null(invr))
    {
      message ("getting cached data")
      return (invr)
    }
    
    ## if there is no data, the function will calculate the output
    data <- x$get()
    invr <- solve(data)
    x$setInv(invr)
    invr
}

## The first function caches the inverse of a matrix. The second checks to see if one 
## is cached and, if so, pulls from the cache.

## makeCacheMatrix takes a matrix as input, generates its inverse, and caches it.

testcase <- replicate(10, rnorm(10))  ## generates random square matrix 
                                      ##  to be used as a test.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  ## the set function caches the inverse of x
    x <<- y             ## cache the matrix
    inv <<- solve(y)    ## cache its inverse

  } 
   
  get <- function() x   ## gets Matrix
  getinv <- function() inv  ## gets inverse
  
  list(set = set, get = get, getinv = getinv)
}


## cacheSolve checks to see if the output of the function above
## has already been cahced, and if so will returned the cached version.

cacheSolve <- function(x, ...) {     
  sol <- x$getinv()                ## Pulls inv from cache 
  if(!is.null(sol)) {              ## Checks to see if data exists
    message("getting cached data")
    return(sol)                     ## If it exists, retunrs cached data
  }
  else {                            ## Otherwise, computes inverse and returns
    sol <- solve(x)
    return(sol)
  }

}

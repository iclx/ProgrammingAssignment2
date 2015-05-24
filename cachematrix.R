##  The following functions implement a caching approach for the retrival of a matrix inverse, 
##  where the repeated recomputation of the inverse is avoided until needed 
##  i.e. until the matrix is changed. 


## The makeCacheMatrix function, creates a quadruple containing functions, 
## that manage the state of the matrix with cached inverse:
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setInverse - set the value of the inverse
## 4. getInverse - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) 
{
      inverse <- NULL
      set <- function(y) 
      {
            x <<- y
            inverse <<- NULL  ## matrix has changed, recomputation of the inverse is needed
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}



## The cacheSolve function returns the inverse of 
## the special matrix with cached inverse,  
## created with the makeCacheMatrix function. 
## The inverse is calculated and written to the cache,
## only if it was not previously, already computed.
## Otherwise, the inverse is retrived from the cached value.
cacheSolve <- function(x, ...) 
{
      inv <- x$getInverse()
      if(is.null(inv))
      {
            ## Inverse is NULL, it must be computed
            matrix <- x$get()
            inv <- solve(matrix)
            x$setInverse(inv)
      }
      inv
}

testCacheMatrix  <- function(x, ...) 
{
      
      if (abs(sum(cacheSolve(x) %*% x$get(), na.rm =TRUE) - nrow(x$get())) < 0.00000001)
      {
            print("Cache matrix test PASSED")
      }
      else
      {
            print("Cache matrix test FAILED")
      }
}


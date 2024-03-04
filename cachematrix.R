## The makeCacheMatrix function creates a matrix object that accepts an 
## input value and clears the cached inverse. It sets and gets the inverse
## and returns a list of the prior defined functions


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y  
    inv <<- NULL  
  }

  get <- function() x
  
  setInverse <- function(inverse) {
    inv <<- inverse  ## Assigns the cached inverse
  }
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}



## This function takes matrix x as input. It retrieves the inverse from 
## the makeCacheMatrix function, if this inverse is not there, the function
## calculates the inverse, if it is there it is being retrieved. Lastly, 
## if the inverse if calculated it is stored in the cache and returned

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    print("retrieving cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}

#the below shows an example of the interactions between the 2 functions

test_matrix <- matrix(c(3, 4, 5, 6), nrow = 2)
makeCacheMatrix(test_matrix)

x = makeCacheMatrix(test_matrix)
cacheSolve(x)

cacheSolve(x)
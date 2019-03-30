## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## A function that enables to cache the inversed values inside the newly created matrix.
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  
  set <- function(y) { ## Setter for the matrix
    x <<- y   
    inv <<- NULL
  }
  get <- function() x  ## Getter for the matrix
  
  setinverse <- function(inverse) inv <<- inverse ## Setter for the matrix inverse
  getinverse <- function() inv ## Getter for the matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## Shows the result which is a list of functions for matrix
}


## Write a short comment describing this function

## A function that computes the inverse of a matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        
  inv <- x$getinverse()
  
  ## Returns cached matrix inverse if done computing
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Compute inverse of matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Cache inverse
  x$setinverse(inv)
  inv
}


#Sample usage:
m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
m2 <- makeCacheMatrix(m)
cacheSolve(m2)

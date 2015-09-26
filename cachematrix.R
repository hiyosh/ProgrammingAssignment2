## makeCacheMatrix: Creates a special "matrix" object which can cache
## the matrix value and its inverse

## cacheSolve: Takes in the special "matrix" object and calculates
## the inverse matrix if it hasn't already been calculated.

## Creates a special "matrix", which is really a list containing
## a function to:
## 1. Set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Clear any prior values
  inverseMatrix <- NULL

  ## Set function sets the new value of the matrix and clears any prior inverse values
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }

  ## Declares the get, setinverse, and getinverse functions, which
  ## merely sets/returns a value without any modification or calculation.
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix"
## created with the above function. It assumes the matrix is
## invertible and solvable via solve()
cacheSolve <- function(x, ...) {
  ## First check to see if the inverse has already been calculated.
  ## If so, it `get`s the inverse from the cache and skips the computation.
  inverseMatrix <- x$getinverse()
  if (!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## Else it calculates the inverse of the data and sets the value of the
  ## inverse in the cache via the `setinverse` function.
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)

  ## Return a matrix that is the inverse of 'x'
  inverseMatrix
}

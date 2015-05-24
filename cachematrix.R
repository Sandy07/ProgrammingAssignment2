
## Create a special "matrix" that can cache it's inverse.
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  newMatrix <- NULL
  set <- function(y) {
    x <<- y
    newMatrix <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) newMatrix <<- solve
  getMatrix <- function() newMatrix
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If inverse has already been calculated, get inverse from cache.
## Otherwise, calculate inverse and set value of matrix in cache via 
## setMatrix function
cacheSolve <- function(x, ...) {

  newMatrix <- x$getMatrix()
  if(!is.null(newMatrix)) {
    message("getting cached data")
    return(newMatrix)
  }
  data <- x$get()
  newMatrix <- solve(data, ...)
  x$setMatrix(newMatrix)
  newMatrix
}

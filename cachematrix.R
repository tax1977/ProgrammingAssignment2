## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getMatrix <- function() x
  getInverse <- function() inverse
  setInverse <- function(inv) inverse <<-inv
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  } 
  list(get=getMatrix,getInv=getInverse,setInv=setInverse,set=setMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv = x$getInv()
  if (!is.null(inv)) {
    print("Inverse already calculated")
    return(inv)
  }
  mat <- x$get()
  if (det(mat)!=0) {
    inv <- solve(x$get())
  } 
  else {
    inv <- NULL
    print("Matrix not invertible")
  }
  x$setInv(inv)
  inv
  
}

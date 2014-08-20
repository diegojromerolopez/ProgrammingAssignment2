# Creates a cache matrix object from a standar R matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initial values:
  # - x stores the original matrix,
  # - m stores the inverse matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Methods of our object
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  # It can be added a method
  # updateinverse that test if the inverse matrix exists
  # and otherwise, creates it. It is not a good practice
  # depending in the cacheSolve external function
  
  # Return our object
  list(matrix=matrix,
       get=get,
       getinverse=getinverse,
       setinverse=setinverse)
}


## Creates the inverse matrix for x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # First, we test if the CacheMatrix object has already
  # computed its inverse matrix. If that's the case, return
  # the inverse matrix
  x_1 <- x$getinverse()
  if(!is.null(x_1)){
    message("getting cached data")
    return(x_1)
  }
  # Otherwise, the inverse matrix is computed and assigned
  # to the internal attribute of the CacheMatrix object,
  # and also, returns this inverse matrix.
  x_1 <- solve(x$get())
  x$setinverse(x_1)
  return(x_1)
}

# Example
test <- function(){
  m <- matrix(c(1,2,3,4), 2, 2)
  x <- makeCacheMatrix(m)
  # First we initialize its inverse matrix
  print(cacheSolve(x))
  # This time, the matrix has its inversed cached
  print(cacheSolve(x))
  # Getting the cached inverse matrix
  print(x$getinverse())
}
#test()

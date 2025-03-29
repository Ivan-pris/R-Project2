## Programming Assignment 2: Lexical Scoping


## Make Cache Matrix Function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache when setting a new matrix
  }
  
  get <- function() x  # Retrieve the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getinverse <- function() inv  # Retrieve the cached inverse
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}



## Cache Solve Function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if inverse is cached
  
  if (!is.null(inv)) {
    message("Getting cached data")  # Inform user that cached data is used
    return(inv)
  }
  
  data <- x$get()  # Retrieve the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setinverse(inv)  # Store the inverse in cache
  
  inv
}

example:

mat <- matrix(c(2, 1, 1, 4), 2, 2)
cachedMat <- makeCacheMatrix(mat)

cacheSolve(cachedMat)
           [,1]       [,2]
[1,]  0.5714286 -0.1428571
[2,] -0.1428571  0.2857143

cacheSolve(cachedMat)
Getting cached data
           [,1]       [,2]
[1,]  0.5714286 -0.1428571
[2,] -0.1428571  0.2857143

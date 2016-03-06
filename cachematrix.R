## The cacheSolve function creates the inverse of a matrix which is then used in the first function makeCacheMatrix. The makeCacheMatrix function creates a matrix. 

#The makeCacheMatrix function creates a matrix and it contains a function to set and get the values of the matrix
#and to set and get the values of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) m <<- solve
  getinvers <- function() m
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


##The function cacheSolve calculates the inverse of the matrix which was created before through the command "solve()". If the inverse has been calculated 
##already, it will get it from the cache.

cacheSolve <- function(x, ...) {
  m <- solve(x)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinvers(m)
  m
}
#  DATA SCIENCE SPECIALIZATION
#  2. R PROGRAMMING
#  DUE 2015.09.27


# makeCacheMatrix: 
#           This function creates a special "matrix" object that can cache 
#           its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
#  SETTING THE MATRIX
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
#  GETTING THE MATRIX
  get = function() x

#  SETTING THE INVERSE
  setinv = function(inverse) inv <<- inverse 

#  GETTING THE INVERSE
  getinv = function() inv
  
#  LISTING
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# cacheSolve: This function computes the inverse of the special 
#           "matrix" returned by makeCacheMatrix above. If the inverse has 
#           already been calculated (and the matrix has not changed), then 
#           the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  inv = x$getinv()

# IF THE INVERSE IS KNOWN... GET CACHE AND RESUME PROCESS
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
# OTHERWISE CALCULATES THE INVERSE OF THE MATRIX 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}

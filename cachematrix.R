## 
# There are two functions, the first of which will create a matrix, as well as some
# steps that allow for the matrix's inverse to be cached. The followup function then will
# either call the cached matrix, or compute the inverse of the cached matrix. So the process
# will be to first supply a matrix to the makeCachematrix function, and save that as an object.
# Then you supply that object to the cacheSolve function in order to get the inverse. These 2 steps
# have now successfully completed the costly computation of getting the inverse, and this computation
# no longer needs to be processed. You can simply type object that was saved with the original 
# makCachematrix function, and add the `$getinverse` to the end in order to call for 
# the already cached inverse, no computation necessary.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matx <- x$get()
    inv <- solve(matx, ...)
    x$setinverse(inv)
    inv     
}

# Run the lines below to test the efficacy of the functions above

pmatrix <- makeCacheMatrix(matrix(sample(16, replace = F), nrow = 4, ncol = 4)) # create a matrix

pmatrix$get() # get the matrix
pmatrix$getinverse()  #inverse has not been computed yet

cacheSolve(pmatrix) #inverse is computed and cached

pmatrix$getinverse() #access cached inverse

           












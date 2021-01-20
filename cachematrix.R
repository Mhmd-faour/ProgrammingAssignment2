makeCacheMatrix <- function(x = matrix()) {   ##defined the function name
  inv <- NULL             ##give inv NULL value
  set <- function(y) {  ## set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}  ##get value of the matrix
  setinverse <- function(solved) { inv <<- solved}  ##set value of the inverse matrix calculated in the next function
  getinverse <- function() {inv}  ##get value of the inverse matrix
  list(set = set, get= get, setinverse = setinverse, getinverse = getinverse) ##created the list of available matrices in the cache
}

## now to solve the inverse and see the data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()   ##see if the value of the inverse is cached and calculated before
  if(!is.null(inv)){   ###if it is NOT null then the value is in the cache and must be retrieved
    message("getting cached data")
    return(inv)   ##retrieved the value from the cache
  }
  data <- x$get()    ##gets the value of the matrix
  inv <- solve(data, ....)  ##inverses the matrix using the solve function 
  x$setinverse(inv) ##sets the value of the inversed matrix into the cache
  inv  ##prints the inv
}

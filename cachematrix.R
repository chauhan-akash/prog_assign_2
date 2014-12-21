## This r script contains function to calculate the inverse of a matrix
## however it also saves the inverse of a matrix in cache and returns
## it to aoid repeated calculation

## this function creates a matrix gets the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  ##initialize a null matrix
  m <- matrix()
  set <- function(y){
    x <<- y
    m <<- matrix()
  }

  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function checks if the inverse of a matrix already exists
## and if it does it caches the inverse else calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## check to see if the inverse matrix already exists
  if(!is.na(m)){
    message("getting cached matrix")
    return(m)
  }
  ## return the inverse of a matrix after calculation
  data <- x$get()
  m <- solve(data,...)
  x$setinv()
  m
}

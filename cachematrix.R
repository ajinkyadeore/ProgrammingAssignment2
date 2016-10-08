## Calculating inverse of a matrix is costly hence instead of finding inverse repeatedly
## it's beneficial to cache the inverse of the matrix which the following
## two functions

## This function stores the inverse in variable "inv" and the matrix to be operated upon
## is stored in variable "mat".It returns a list whose members are assigned to a function.
## "mat" is set through the "set" function and the cache is cleared by equating "inv" to null.
## "setinv" function takes the inverse as an argument and sets it to "inv".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function takes a matrix as an argument, sets it and then checks if inverse has been calculated and
## cached. If cached, it  displays the inverse from the cache else it calculates the inverse and sets the
## cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}

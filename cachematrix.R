##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
   set <- function(y) {
    x <<- y    
    z <<- NULL 
  }
   get <- function() x
  set_inverse <- function(inverse) 
    z <<- inverse
  get_inverse <- function() z
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x) {
  z <- x$get_inverse() 
  if(!is.null(z)) {
    message("Getting cached data.")
    return(z)
  }
  
  data <- x$get()  
  z <- solve(data) 
  x$set_inverse(z)  
  z               
}

## Typing the following will produce the results of the desired program.
## x = rbind(c(1, -1.5), c(-4, 1))
## z = makeCacheMatrix(x)
## z$get()
## cacheSolve(z)
## cacheSolve(z)

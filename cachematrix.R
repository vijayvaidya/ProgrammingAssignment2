## This function takes a matrix as an argument.
## Builds a list of components, set,get,setInverse,getInverse
##   "set" is to set the cached value
##   "get" returns cached value
##   "setInverse"  sets cached inverse value
##   "getInverse" - returns cached inverse value
##
## Sample R session from R-Studio
##  x <- matrix(c(1:10), nrow =2, ncol = 2)
##  > x
##     [,1] [,2]
##     [1,]    1    3
##     [2,]    2    4
##  > y <- makeCacheMatrix(x)
##  > io <- cacheSolve(y)
##  > io <- cacheSolve(y)
##  getting inverse from cache
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y      # cache the value 
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the x as an argument which is a list that
## contains matrix to inverse and set/get functions
## Calls getInverse to get the inverse from cache if at all there
## If exixts in cache then returns it. If not the computes the 
## inverse and set it in cache by calling "setInverse"
##

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting inverse from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)    #Compute the inverse
  x$setInverse(m)          #Set it in cache
  m                        # Returns the inverse
}

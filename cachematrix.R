##Create Matrix in order to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##Returns the inverse from the cache if not already cached;
##If already cached and matrix not changed, returns that inverse

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
  m
}
  
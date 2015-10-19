# Function cacheSolve: Returns the inverse of a matrix and stores it if has not been calculated before.
# First expand your regular matrix using the makeCacheMatrix function on it.

# This function expands matrix to support caches.  
makeCacheMatrix <- function(u = matrix()) {
  inv <- NULL
  set <- function(v) {
    u <<- v
    inv <<- NULL
  }
  get <- function() u
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

# casheSolve returns the inverse of a matrix if cashed or calculates the inverse.
cacheSolve <- function(u, ...) {
  inv <- u$getinv()
# If inverse matrix exists returns it.
  if(!is.null(inv)) {
# Message("Found inverse in cache.")
    return(inv)
  }
# Calculates inverse and stores it.
  mat <- u$get()
  inv <- solve(mat)
  u$setinv(inv)
  return(inv)
}


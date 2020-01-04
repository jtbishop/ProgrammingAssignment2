# This function creates an object pointing to the inverse of 
# a matrix's inverse. Warning: Will set any variable named 'm' 
# to NULL
makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL
  
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  
  get = function() x 
  set_inverse = function(inverse) m <<- inverse
  get_inverse = function() m
  
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}

# Takes a cached matrix as an input and returns the 
# inverse of the matrix. If the matrix's inverse
# has already been found, it will retrieve it from
# memory and notify the user.
cacheSolve <- function(x, ...) {
  m = x$get_inverse()
  
  if(!is.null(m)) {
    message('Getting cached matrix inverse.')
    return(m)
  }
  
  data = x$get()
  m = solve(data, ...)
  x$set_inverse(m)
  m
  
}

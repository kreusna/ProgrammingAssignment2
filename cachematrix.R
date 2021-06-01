## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## m to NULL
  m <- NULL
  ## create set() and store matrix to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## create get() return x
  get <- function() x
  
  ## create setInverse() and set inverse matrix to m
  setInverse <- function(inverse) m <<- inverse 
  
  ## create getInverse() and return m
  getInverse <- function() m
  
  ## create list for function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get inverse matrix 
  m <- x$getInverse()
  
  ## check if inverse matrix is set
  if(!is.null(m)){
    return(m)
  }
  
  ## get matrix 
  data <- x$get()
  
  ## matrix is inverse 
  m <- solve(data)
  
  ## set inverse matrix
  x$setInverse(m)
  
  ## return inverse matrix
  m
}

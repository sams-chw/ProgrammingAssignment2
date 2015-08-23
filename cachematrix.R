# A predefined matrix is supplied while the makeCacheMatrix function is created:
# a  <-  matrix(c(sample(1:50,4),sample(1:50,4),sample(1:50,4),sample(1:50,4)), nrow = 4, ncol = 4)
# x <- makeCacheMatrix(a)
# The supplied matrix can now be modified by accessing the following function:
# x$setMatrix(matrix(c(sample(1:10,2),sample(1:10,2)), nrow = 2, ncol = 2))
# cacheSolve function is now called with the output of makeCacheMatrix function as its argument 
# to obtain the inverse of the matrix either from the cached value or by calculating it:
# cacheSolve(x)

makeCacheMatrix <- function(x = matrix()) {
  # initially cache is set to NULL
  cache <- NULL
  
  # x is modified as a new matrix is supplied 
  setMatrix <- function(xNew) {
    x <<- xNew
    # since a new matrix is assigned to x, the stored cache is cleared
    cache <<- NULL
  }
  
  # the matrix is returned
  getMatrix <- function() x
  
  # the inverse of the matrix is cached
  setInverse <- function(inverseMatrix) cache <<- inverseMatrix
  
  # the cached value is returned
  getInverse <- function() cache
  
  # return a list of the functions is returned
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix, 
    setInverse = setInverse, 
    getInverse = getInverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # the cached value is fetched
  inverseMatrix <- x$getInverse()
  print(inverseMatrix)
  
  # the cached is returned if exists
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # otherwise the matrix is fetched and its inverse is determined
  
  data <- x$getMatrix()
  print(data)
  
  # Althought the assignment does not require to test the invertibility of the supplied matrix
  # it is nonetheless a good idea to take a precaution by checking if inverse of the matrix exists
  
  inverseMatrix <- tryCatch(round(solve(data),6), error = function(e) e)
  
  if(any(class(inverseMatrix) == "error")){
    print("Singular Matrix")
    inverseMatrix <- NULL
  }
  
  # once the inverse matrix is determined, it is now cached
  x$setInverse(inverseMatrix)
  
  # finally, the inverse matrix is returned as an output
  inverseMatrix
  
}

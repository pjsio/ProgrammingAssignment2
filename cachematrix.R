## There are two functions here
## 1. makeCacheMatrix takes a square invertible matrix as its input. and outputs a list of functions.
## 2. cacheSolve takes the output from makeCacheMatrix as its input, and outputs its inverse.
## !!!! inputting a matrix into cacheSolve directly will produce an error !!!
##      good example: cacheSolve(makeCacheMatrix(matrix(c(0,0,0,0), nrow=2, ncol=2)))
##      bad  example: cacheSolve(matrix(c(0,0,0,0), nrow=2, ncol=2))

## 1. makeCacheMatrix takes a square invertible matrix as its input.
##    This function caches a list of subordinate functions that will be called by cacheSolve later
##    - get caches the input matrix
##    - set_inverse caches the inverse in 'i' - due to the '<<-' operator, it is possible for
##      an outside function like cacheSolve to assign a value into the original cache
##    this function also checks whether the input matrix is square invertible - although it doesn't stop
##    the user from inputting a non-square matrix, it does spit out an error message in such a case.

makeCacheMatrix <- function(x = matrix()) {
  if(!dim(x)[1]==dim(x)[2]){
    message("the matrix entered is not a square invertible matrix. cacheSolve will run into an error.")
  }
  i <- NULL
  get <- function() x
  
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  
  list(get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## 2. cacheSolve takes a square invertible matrix, and tries to find its inverse.
##    - but before that, it checks the get_inverse cache for an existing inverse value
##    - if it finds the inverse, it outputs that first
##    - if it doesn't find the inverse, it
##        (1) solves the matrix for its inverse
##        (2) sets the inverse using set_inverse with that solution
##        (3) and outputs that value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  if (!is.null(i)){
      message("inverse already cached - getting cached data")
      return(i)
  }
  else {
      data <- x$get()
      i <- solve(data, ...)
      x$set_inverse(i)
      i
  }
}
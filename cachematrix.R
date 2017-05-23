## Below are functions to calculate the inverse of a matrix by checking its cache first.
## makeCacheMatrix is a function to set and get the value of a matrix and its inverse. Then
## it used in cachematrix to calculate the inverse of given matrix

## makeCacheMatrix defines a new type of matrix, by returning a list
## of functions to set and get the matrix and its inverse. The input is a matrix, and
## the output is a list
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cachematrix is a function that returns the inverse of a matrix by
## checking whether the inverse matrix has been cached or not. If yes, value
## is returned from cache. If no, it is solved by solve function and it is
## saved to cache. Input is a matrix to be calculated its inverse matrix.
## Output is the inverse matrix if it has been cached, or NULL if it has not

cachematrix <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

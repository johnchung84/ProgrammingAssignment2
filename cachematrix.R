## This Rscript file contains 2 functions used to create a cache matrix
## and to either compute the inverse of the matrix or to retrieve it from 
## cache if it was computed already and stored in cache. 

## FUNCTION - makeCacheMatrix
## This function takes matrix x and stores it in cache by assigning the values
## of the object in an environment that is different from the current one. 

## This function has 4 subfuctions assigned to it. The subfunctions are the following:
##    set, get, setsolve, getsolve
## Returns a list of those 4 functions. 

## Assumption: The argument x is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  # This function is assigning value y to the replace the value of x originally assigned 
  # the object initially created. 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # This function prints the values of x
  setsolve <- function(sol) m <<- sol #This function will be used to assign the solved inverse of x
  getsolve <- function() m #This function prints the values of x inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## FUNCTION - cacheSolve
## This function computes the inverse of the matrix x. Then it stores the inverse matrix into
## the object defined by the makeCacheMatrix function above. 

## This function will first search to see if there is already the inverse of the matrix x
## stored in the list object. If yes, it will output that value. If no, solve function is
## used to compute the inverse of the matrix and then cached to the list object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

### R Programming: Programming Assignment 2
### Jorien van den Bergh

#################################################################################
########################### makeCacheMatrix #####################################
#################################################################################


## This function creates a special matrix object that can cache its inverse.

## Function input: x needs to be a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # Set inverse matrix to NULL
  m <- NULL
  
  # Create function that resets the matrix to a new matrix (and the inverse back to NULL)
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  
  # Create function that returns the matrix x
  get <- function() x
  
  # Create function that sets the inverse of x
  setinverse <- function(inverse = matrix()) m <<- inverse
  
  # Create function that returns the inverse of x
  getinverse <- function() m
  
  # Create list with all four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#################################################################################
########################### cacheSolve ##########################################
#################################################################################


## This function returns the inverse of the special matrix returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), the function will retrieve the inverse from the cache.

## Function input: y is the output of makeCacheMatrix(x)

cacheSolve <- function(y) {
  
  # Check if there already is an inverse calculated and if so, retrieve it
  m <- y$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If not, calculate inverse of the original matrix, set it in the cache and return it
  data <- y$get()
  m <- solve(data)
  y$setinverse(m)
  m
  
}



#################################################################################
##################   Using the functions     ####################################
#################################################################################

# This is an example on how to use the functions defined above on a square invertible matrix.

a <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2)

b <- makeCacheMatrix(a)

cacheSolve(b) #first run will calculate the inverse and cache it
cacheSolve(b) #second run will retrieve the inverse from the cache without calculating again

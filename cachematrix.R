## The two following functions perform management of matrices and inverted matrices
## with the goal of preserving computational time by storing matrices and their associated
## matrixes in the local environment of the function to prevent duplicate computation


## This makeCacheMatrix function returns a list object with fucntions 
## related to matrix managmenent and provides four activities:

## set(): Setting the value of a matrix
## get(): Returning the value of a matrix
## setinverse: Providing the inverse value of a matrix
## getinverse: Returning the inverse value of a matrix

## Note no mathematical calculations are performed in the first function regarding the solving of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing the inverse value and assigning it NULL
  m <- NULL
  
  ## First function which sets the matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Second function which returns the associated matrix
  get <- function() x
  
  ## Third function sets the inverse values of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## Fourth function returns the inverse value of a matrix
  getinverse <- function() m
  
  ## List object returned with functions from above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This function test to see if an inverse of a matrix created and stored in the fucntion named makeCacheMatrix
## has been calculated. If the inverse of the matrix has been calculated it retrieves the values of the inverse matrix.
## If the inverse has not been calculated it calculates the inverse.

cacheSolve <- function(x, ...) {
  
  ## Assigning a variable (m) the value of the inverse matrix. Note this could be NULL if the value was not already
  ## assigned in the above function.
  m <- x$getinverse()
  
  ## Checking if the variable (m) is Null which communicates the inverse was not calculated. if the value is not null
  ## (e.g. the inverse value is already stored) then it returns the value and ends the function.
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    return(m)
  }
  
  ## As the 'return' function above ends the function and the if statement has only one condition this code is 
  ## executed to solve for the inverse.
  
  ## Retrieving the value of the matrix
  data <- x$get()
  
  ## Solving for the inverse of the matrix and assigning to variable m
  m <- solve(data, ...)
  
  ## Setting the inverse value since we just calculated it and want to conserve computation cycles 
  ## (e.g. so we won't need to calculate it again)
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
  
}

## Example Usage:

# # Create test matrix
#      test <- matrix(c(1,3,2,4),2,2)
#      test
# 
# # Instantiate inv object associated with test matrix from function:
#      inv<-makeCacheMatrix(test)
# 
# # Get the value of the matrix
#      inv$get()
# 
# # Get the value of the inverse of the matrix (NOTE: this should be NULL as none provided so far)
#      inv$getinverse()
# 
# # Check if matrix has associated inverse solved ("getting cached inverse matrix data" NOT printed)
# # will return value of inverted matrix
#      cacheSolve(inv)
# 
# # Perform again and see that the value is cached (evidenced by "getting cached inverse matrix data" printed )
#      cacheSolve(inv)


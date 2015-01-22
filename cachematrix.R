## These are the functions that you can use in order
## to set a matrix (squared), calculate and cache its inverse
## I've used the same structure of the example but
## removing (or commenting) a couple lines that are not
## necesarry for the procedure and changing variable names

makeCacheMatrix <- function(mat = matrix()) {
  invmat <- NULL #Initializes the invmat variable as null
  
  #I've commented these lines because seti is not going to be used
  #seti <- function(y) {
  #  mat <<- y
  #  invmat <<- NULL
  #} 
  
  #This method will be called by cacheSolve to get the initial matrix
  geti <- function() mat 
  #This method will be called by cacheSolve to set and cache the inverse of the matrix
  sinv <- function(solve) invmat <<- solve
  #This method will be called by cacheSolve to get the cached matrix
  ginv <- function() invmat 
  list(geti = geti,
       sinv = sinv,
       ginv = ginv) #I've removed seti = seti because is not going to be used
}

## This function extracts the cached values, if there is no cached
## values then the function does the procedure.
## It's important to set the x argument as an object (List) created
## previously with the function makeCacheMatrix 

cacheSolve <- function(x, ...) {
  #uses ginv()  to consult any cached inverse matrix
  invmat <- x$ginv() 
  #If there is a matrix just return that matrix
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  # These couple lines will be performed in case the previous condition 
  # doesn't match
  result <- x$geti() # Calls the geti method and gets the initial matrix
  invmat <- solve(result, ...) #Proceeds to calculate de inverse of 'x'
  x$sinv(invmat) #Calls the sinv method to set de values of the inverse of 'x'
  invmat ## As the last line it will return a matrix that is the inverse of 'x' 
}

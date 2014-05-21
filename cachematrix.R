## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverseMatrix
  
  inverseMatrix <- NULL 
  
  # -----------------------------------------------------------------------------------------
  # set function : sets the passed Matrix as the new data and resets the cached inverseMatrix
  set <- function(replacedMatrix) {
    x <<- replacedMatrix 
    inverseMatrix <<- NULL 
  }
  
  
  # -----------------------------------------------------------------------------------------
  # get function: Get the corresponding matrix for which the inverse is to be calculated. 
  get <- function() {
    x
  }
  
  
  # -----------------------------------------------------------------------------------------
  # getinverse: Get the corresponding inverse matrix for which the inverse is to be calculated. 
  getInverse <- function() {
    inverseMatrix
  }
  
  
  # -----------------------------------------------------------------------------------------
  # setinverse: Store the corresponding inverse matrix 
  setInverse <- function( passedInverse) {
    inverseMatrix <<- passedInverse 
  }
  
  list ( get=get,set=set,getInverse=getInverse, setInverse=setInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  localInverseMatrix <-  x$getInverse()
  if( !is.null(localInverseMatrix)) {
    message("Getting the inverse of the Matrix from the Cache")
    return(localInverseMatrix)
  }
  
  originalMatrix = x$get()
  
  computedInverse = solve(originalMatrix,...)
  
  x$setInverse(computedInverse)
  
  computedInverse
  
  
}


assignment2Unittest <- function() {
  
  #
  # to run the unit test, do the following command on R Prompt
  # > source("cachematrix.R") 
  # > assignment2unittest()
  #
  
  #
  # 3x + 2y - 5z =  12
  # x - 3y + 2z = -13
  # 5x -  y + 4z =  10
  # the matrix is derived for the above mentioned equations. 
  
  
  
  
  data <- c(3.0,2.0,-5.0,1.0,-3.0,2.0,5.0,-1.0, 4.0)
  originalMatrix = matrix(data,nrow=3,ncol=3,byrow=TRUE)
  
  message("Original Matrix is printed below")
  print(originalMatrix)
  
  specialMatrix <- makeCacheMatrix(originalMatrix)
  
  InverseMatrix <- cacheSolve(specialMatrix)
  
  # should see the message saying that the data is from cache. 
  InverseMatrix <- cacheSolve(specialMatrix)
  
  message("The computed Inverse matrix is printed below")
  print(InverseMatrix)
  
  message("The verification of correctness starts")
  
  # now verify  MI %*% M == I 
  
  mi =  specialMatrix$getInverse()
  m  = specialMatrix$get()
  
  checkMatrix = round(mi %*% m,5) 
  
  message("The computed Inverse * Original   is printed below, diagonal Elements expected to be 1")
  print(checkMatrix)
  
  # subtract 1 from all diagonal elements to make the result vector all zeros
  
  checkMatrix[1,1] <- checkMatrix[1,1] - 1 
  checkMatrix[2,2] <- checkMatrix[2,2] - 1 
  checkMatrix[3,3] <- checkMatrix[3,3] - 1 
  
  # check if all the elements of the vector is 0 , if all zero then the inverse is correct else error
  
  stopifnot(checkMatrix[,]==0)
  
  # will not reach here if the test fails ( i.e the I vector is not correct)
  # hence the test passed. 
 
  message("test Passed")
  
}

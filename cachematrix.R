# -------------------------------------------------------------------------------------------------------
#
#   File:  cachematrix.R
# 
#   Synopsys: This file contains two utility functions and one unit test function, these are 
#          explained below
# 
#         a)  makeCacheMatrix(): function to create a special Matrix class which 
#                   provides the content for storing and retrieving the inverseMatrix once it is computed. 
#         
#         b) cacheSolve():  function which either 
#               computes & caches the inverse of the matrix in 
#                  underlying special matrix
#              -or 
#               retrieve the cached inverse Matrix which was already computed earlier.  
#               In both the cases, a solved inverse matrix is returned to the user. 
#         
#         c) assignment2Unittest():  The unit test functions which demonstrates the calling sequence
#         of the functions makeCacheMatrix() and cacheSolve(). The unit test computes the 
#         inverse of 3x3 matrix and recheck by multiplying this to the original matrix
#         to get an the Identity matrix. This identity matrix is then checked if it is really an identity matrix.
# 
#



#===============================================================================
# makeCacheMatrix() : Takes a invertible matrix and creates a special matrix class which 
# additionally stores an cached computed inverse matrix. 
# 
#   Input parameters =  x   a matrix which is assumed to be invertible.  
# 
#   returned parameters : a Special Matrix which can store/retrieve the inverseMatrix. 
# 
# Description: the function takes a matrix ( which is assumed to be invertible) and 
# creates a special Matrix class which additionally allows inverse matrix to be cached and kept. 
# The class provides the set and get method for the basic matrix which was  passed during the creation, 
# the operator <<- is used to check for the variable in scope before  deciding to update the 
# existing variable or create a new variable. This lexical scoping helps to have a caching of variable. 
#  When the set method is called, then the cached inverse matrix is reset back to NULL.
# 
#    Functions
#           get()  : returns the original Matrix
#           set(newmatrix) :  sets the new matrix, this resets the cache for the inversematrix to NULL
#           getInverse() returns the inverseMatrix if cached or NULL if not cached. 
#           setInverse(inverseMatrix) : sets the cache to the passed inverseMatrix 
#



makeCacheMatrix <- function(x = matrix()) {
  # cached inverseMatrix
  
  inverseMatrix <- NULL 
  
  # -----------------------------------------------------------------------------------------
  # set function : sets the passed Matrix as the new data and resets the cached inverseMatrix to NULL
  set <- function(replacedMatrix) {
    x <<- replacedMatrix 
    inverseMatrix <<- NULL 
  }
  
  
  # -----------------------------------------------------------------------------------------
  # get function: Gets the original matrix for which the inverse is to be calculated. 
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


#===============================================================================
# cacheSolve() : Takes the special matrix created earlier as a parameter and computes the inversematrix by
# calling function "solve()", the extra parameters are passed to the "solve()" funciton. the function
# caches the computed inverse matrix in the underlying special matrix which was passed as parameters. 
# additiona
# 
#   Input parameters =  x   the special matrix which was earlier created by makeCacheMatrix()   
# 
#   returned parameters : a inverse matrix 
# 
# Description:  The algorithm functions as follows 
# 
#       - retrieves the inverse from the special matrix to check if inverse was cached ( Non NULL).
#       -  if it is already cached, then return the cached version to the user
#       - if it is not cached, then inverse is computed and cache is updated 
#       - finally the computed inverse is returned to the user. 
# 
#   
#


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  localInverseMatrix <-  x$getInverse()
  
  # check if it is already cached then return the cached value. 
  if( !is.null(localInverseMatrix)) {
    message("Getting the inverse of the Matrix from the Cache")
    return(localInverseMatrix)
  }
  
  # get the original matrix and compute the inverse. 
  
  originalMatrix = x$get()
  computedInverse = solve(originalMatrix,...)
  
  # set the cache to the computed inverse matrix. 
  
  x$setInverse(computedInverse)
  
  # return the computed value to the user also. 
  
  computedInverse
  
}



#==============================================================
#  Unit test
#
#

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
  
  
  
  # hand craft the original Matrix. 
  data <- c(3.0,2.0,-5.0,1.0,-3.0,2.0,5.0,-1.0, 4.0)
  originalMatrix = matrix(data,nrow=3,ncol=3,byrow=TRUE)
  
  message("Original Matrix is printed below")
  print(originalMatrix)
  
  
  # create a special matrix
  
  specialMatrix <- makeCacheMatrix(originalMatrix)
  
  # calculate the inverse 
  
  InverseMatrix <- cacheSolve(specialMatrix)
  
  
  # try to retrieve again, it should return from the cache. 
  # should see the message saying that the data is from cache. 
  InverseMatrix <- cacheSolve(specialMatrix)
  
  message("The computed Inverse matrix is printed below")
  print(InverseMatrix)
  
  message("The verification of correctness starts")
  
  # now verify  MI %*% M == I 
  
  mi =  specialMatrix$getInverse()
  m  = specialMatrix$get()
  
  
  # round it
  checkMatrix = round(mi %*% m,5) 
  
  message("The computed [] Inverse * Original ] is printed below, diagonal Elements expected to be 1")
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

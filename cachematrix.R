## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## These functions are to be used together
## to cache the inverted matrix of a matrix
## so that it doesn't need to be solved a second time.
##

## SAMPLE CALL and output of simple invertable matrix
##
## > x
## [,1] [,2]
## [1,]    1    1
## [2,]    2    3
## > rm(z)
## > x <- matrix(c(1,2,1,3), 2,2) ##Define a matrix
## > z <- makeCacheMatrix(x) # Call makeCacheMatrix with the Matrix
## > source('C:/Users/Alexander/Dropbox/Rdev/a2/cachematrix.R')
## > cacheSolve(z)
## [,1] [,2]
## [1,]    3   -1
## [2,]   -2    1
## > cacheSolve(z)
## getting cached data
## [,1] [,2]
## [1,]    3   -1
## [2,]   -2    1
## 
## End Sample Call and Output
 
## 
## makeCacheMatrix
## This function has several internal functions
## That will be called by a later funtion
## so that potentially large matrices that need
## to be inverted - can be inverted on a separate thread
##
## Initialize the s variable
##
## set function declaration.  External x varialbe is set
## equal to y and s is re-initialized.
## However, this function is not explicitly called
## in the accompanying cacheSolve function.  The set
## function is called when the makeCacheMatrix is
## is created/assigned to a variable.
##
## get function call declaration.  It returns the contents
## of the x variable.  This is the matrix that
## needs to be processed.
##
## setsolve - performs the solve function against 
## matrix originally sent in the creation of the object
## s is initialized and given the value of the solve
## function call
##
## getsolve - returns s - the inverted matrix
##
## the list is what is returned in the original call
## to this function creating an object (list) with
## the functions declared.
##
makeCacheMatrix <- function(x = matrix()) {
        # initialize the s variable - or set it to NULL
        # if there is a value already there
        s <- NULL
        
        # This set function is called when this variable
        # /object is created.  Stores the original
        # matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # returns the original matrix
        get <- function() x
        
        # solves the matrix to get inverted matrix
        # returns inverted matrix
        setsolve <- function(solve) s <<- solve
        
        # returns NULL or previously calculated
        # inverted matrix
        getsolve <- function() s
        
        # the list returned by the original call
        # creating a list object from this function
        # assigning the names of internal functions
        # to functionality of classes
        list(set = set, get = get,
             getsolve = getsolve,
             setsolve = setsolve)        
}


## Write a short comment describing this function
## The cacheSolve function is called and it makes a few
## calls to the makeCacheMatrix Function
## The first call is to find out if the inverted matrix
## has already been created.  If it has then it
## returns the previously calculated inverted matrix
##
## If there is no previous solution, the get function
## is called from the object sent in which was created by
## makeCacheMatrix.  The get function returns the matrix
## to be inverted.
## the s variable is overwritten by the solve function
## called with the data from the get function.
## the setsolve function is called with the inverted
## matrix created in the previous line, assuming the
## matrix could be inverted.
## This function then returns the inverted matrix 
## the contents of the s variable.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## call input parameter's getsolve function
        ## if this has been previously called
        ## inverted x matrix will be returned
        s <- x$getsolve()
        
        ## if the getsolve() call returned a value
        ## return the inverted matrix with a message
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## call input parameter's get function
        ## getting the original matrix
        data <- x$get()
        
        ## calling the solve function which inverts
        ## the supplied data (a matrix) in to the s
        ## variable
        s <- solve(data, ...)
        
        ## stores the solution so that next time
        ## you can call getsolve() and get the inverted
        ## matrix without repeating the solve function
        x$setsolve(s)
        
        s   ## the value returned - inverted matrix
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
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
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

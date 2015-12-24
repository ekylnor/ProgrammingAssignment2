########################################################################
## PROGRAMMING ASSIGNMENT 2: LEXICAL SCOPING 
## Coursera R Programming course
## Student: ekylnor
##
## This source file contains two functions to calculate and cache the
## inverse of a matrix: 
## - makeCacheMatrix is used to store a matrix and its inverse and 
##   provides internal methods to set those values and to access to them
## - cacheSolve is used to compute the inverse of the matrix
##   (or get it from the cache if it is already available there).
########################################################################

## FUNCTION: makeCacheMatrix
## Input: a matrix (optional)
## Output: returns a list containing four functions:
##    set: to set the value of the matrix
##    get: to get the value of the matrix
##    setInverse: to set the value of the inverse
##    getInverse: to get the value of the inverse
## Notes: 
##    - When setting the value of the matrix, 
##      its inverse is NOT automatically calculated, 
##      but it is set to NULL.
##    - setInverse does NOT calculate the inverse, 
##      but it sets it to the value provided

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(invMatrix) inverseMatrix <<- invMatrix
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## FUNCTION: cacheSolve
## Input: a list created with the makeCacheMatrix function
##        (and optionally additional arguments to be passed 
##        to the R solve() function)
## Output: the inverse of the matrix stored in the list provided as input
## Operation: It first checks if the inverse has already been calculated. 
##    If so, it gets the inverse from the cache and skips the computation. 
##    Otherwise, it calculates the inverse of the matrix and sets the value 
##    of the inverse in the cache via the setInverse function.
## Notes:
##    - It does not check if the matrix is invertible 
##      (it assumes it is according to the instructions of the assignment)
##    - It does not check the extra arguments passed to the R solve() 
##      function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    matrixToBeInversed <- x$get()
    inverseMatrix <- solve(matrixToBeInversed, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}

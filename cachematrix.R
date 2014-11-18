## Date: 11/17/2014
## Author: Nicholas McDannald
## Name: R Programming Assignment 2
## Description: This solution involves 2 functions where the end results
## is to take a matrix, cache the inverse and return the inversed matrix

## NOTE: The structure of the functions are based on the example functions
## makevector() and cachemean() provided by the instructor. This code has been
## tailored to handle matrixes and doing the inverse of a matrix.

## makeCacheMatrix() sets up the ability for function cacheSolve() 
## to cache and retreive the inverse of a matrix supplied externally 
makeCacheMatrix <- function(x = matrix()) {

        #set invmat to nothing. Will store the inverse of the supplied matrix
        invmat <- NULL   
        
        # Used for debugging or other purposes to save and reset invmat
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }

        get <- function() x  # Returns the the original matrix value
        
        # store the inverse matrix in cache
        setInverseMatrix <- function(InverseMatrix) invmat <<- InverseMatrix
                                         
        
        getInverseMatrix <- function() invmat  #return the cached inverse matrix
        
        # This list function is used for the purpose of the cacheSolve() 
        # function to know how to use the functions in the list for calculations
        list(set = set, get = get,      
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
}


## cacheSolve function calculates the inverse of the matrix created 
## with the makeCacheMatrix(). It first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the mean in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
        
        # Get the inverse matrix from makeCacheMatrix() (if cached)
        invmat <- x$getInverseMatrix()
        
        # If cached inverse matrix exists already, return the value and exit
        if(!is.null(invmat)) {
                return(invmat)
        }
        
        data <- x$get() # store original matrix in data
        
        invmat <- solve(data, ...) # calculate the inverse of data
        
        x$setInverseMatrix(invmat) # store the inverse matrix in cache

        invmat # Output the inverse matrix
}

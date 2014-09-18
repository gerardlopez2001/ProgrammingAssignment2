## makeCacheMatrix and cacheSolve are two related functions that work together to
## allow the user to store a square matrix and calculate and store its inverse.
## To reduce computation time, the functions will use a cached version of the 
## inverse of the matrix if one already exists instead of recalculating.

## makeCacheMatrix contains the functions necessary to hold a square matrix and 
## its inverse.

## author:  Gerard Lopez
## date: 2014-09-18
## based on makeVector and cacheMean code written by Roger D. Peng, PhD
##      , Jeff Leek, PhD, Brian Caffo, PhD

makeCacheMatrix <- function(x = matrix()) {
        
        #validate that the incoming matrix is square, if not, stop
        if (ncol(x)!=nrow(x)) {
                stop("Matrix must be square!")
        }
        else {
                
        #set variable invMatrix to NULL to start       
        invMatrix <- NULL
        
        #set function sets x to the argument y and set invMatrix to null
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        #get returns the value of x (argument of makeCacheMatrix)
        get <- function() x
        
        #sets setInvMatrix in makeCacheMatrix to inverse of x matrix (argument of makeCacheMatrix)
        setInvMatrix <- function(solve) invMatrix <<- solve
        
        # getInvMatrix returns the value of invMatrix (from makeCacheMatrix)
        getInvMatrix <- function() invMatrix
        
        #returns a labeled vector of functions set, get, setInvMatrix and getInvMatrix
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
        }
}


## cacheSolve will return a matrix that is the inverse of 'x'.  If this inverse 
## already exists it return cached version instead of re-calculating.

cacheSolve <- function(x, ...) {
        
       
        #attempts to get the inverse matrix from x (if it was calculated previously)
        invMatrix <- x$getInvMatrix()
        
        #if not null, a value was cached, so return invMatrix instead of recalculating 
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        
        #since its null, set data to x from makeCacheMatrix
        data <- x$get()
        
        #calculate the inverse of the matrix
        invMatrix <- solve(data)
        
        #set invMatrix in x to calculated inverse of the matrix
        x$setInvMatrix(invMatrix)
        
        #return inverse of the matrix
        invMatrix        
}
# Assignment: Caching the Inverse of a Matrix
# Write 2 functions (makeCacheMatrix & cacheSolve) that cache the inverse of a matrix

# Examples by Roger D. Peng were used in this work 
# https://github.com/rdpeng/ProgrammingAssignment2

# makeCacheMatrix: This function creates a special "matrix" object 
# used by cacheSolve to get or set the inverted matrix in cache
# Create a special "matrix", which is a list containing a function to
#   - set the value of the matrix
#   - get the value of the matrix
#   - set the value of the inverse matrix
#   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # stores the cached value
    cache <- NULL # initialize to NULL
    
    # create the matrix in the working environment
    # the <<- operator is used to assign a value to an object in the working environment
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # invert the matrix and store in cache
    setInverse <- function(inverse) cache <<- inverse
    
    # get the inverted matrix from cache
    getInverse <- function() cache
    
    # Return the created functions to the working environment
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then cacheSolve should retrieve the inverse from the cache 
# and skip the computation. Otherwise, it calculates the inverse and sets the value 
# of the inverse in the cache via the setInverse function.
# For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    # attempt to get the inverse of the matrix stored in cache
    cache <- x$getInverse()
    
    # return inverted matrix from cache if it exists
    if (!is.null(cache)) {
        message("getting cached data")
        # display cached inverted matrix in console and finish
        return(cache)
    }
    
    # else create the matrix in working environment since it does not exist
    matrix <- x$get()
    
    # set and return inverse of matrix
    cache <- solve(matrix, ...)
    
    # set inverted matrix in cache
    x$setInverse(cache)
    
    # Return a matrix that is the inverse of 'x'
    return(cache)
}

# Usage example:
# source('cachematrix.R')
## try simple matrix:
# > m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), c(2, 2)))
## or
# > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## or
# > x = rbind(c(1, 3), c(2, 4))
# > m = makeCacheMatrix(x)
## display that matrix
# > m$get()
##       [,1]  [,2]
## [1,]    1    3
## [2,]    2    4
## Solving for inverse
# > cacheSolve(m)              1st run returns inverted matrix
##                              from working environment
##      [,1]  [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
# > cacheSolve(m)              2nd and subsequent runs
#                              returns inverted matrix from cache
## getting cached data
##      [,1]  [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

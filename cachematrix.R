## These functions store and then solve for the inverse of a matrix.

## The makeCacheMatrix creates a list of four functions(set.matrix, get.matrix,
## set.inverse and get.inverse).  These functions do the following:
## set.matrix: Changes the matrix stored in the main function;
## get.matrix: Returnes the matrix stored in the main function;
## set.inverse: Stores the value of the input matrix into the main function;
## get.inverse: Returns input matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set.matrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get.matrix <- function() x
    set.inverse <- function(inverse) inv <<- inverse
    get.inverse <- function() inv
    list(set.matrix = set.matrix, get.matrix = get.matrix,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## This function solves for the inverse of the given matrix from makeCacheMatrix
## and then returns it to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get.inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get.matrix()
    inv <- solve(data, ...)
    x$set.inverse(inv)
    inv
}

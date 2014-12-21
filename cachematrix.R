## creating a matrix object to return matrix inverse
## idea is to store inverse and hence avoid repeated matrix inverse 
## calculations

makeCacheMatrix <- function(X = matrix()) {
	## X is a numerical, square invertible matrix
	## this function creates a matrix object that can be used to cache 
	## matrix inverse
        invX <- NULL
        set <- function(y) {
                X <<- y
                invX <<- NULL
        }
        get <- function() X
        set_inv <- function(solve) invX <<- solve
        get_inv <- function() invX
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

cacheSolve <- function(Xo, ...) {
	## Xo is a matrix object created by makeCacheMatrix
	## this function returns cached inverse of a matrix if it exists
	## or calculates it is it does not exist
        invX <- Xo$get_inv()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- Xo$get()
        invX <- solve(data, ...)
        Xo$set_inv(invX)
        invX
}
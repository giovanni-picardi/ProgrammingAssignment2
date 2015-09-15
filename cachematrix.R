## makeCacheMatrix
## creates a special matrix object capable of storing the matrix data as well as
## the inverse of the matrix
##
## cacheSolve
## computes and sets, or retrieves if already computed, the inverse of a matrix
## object created with the previous function

## makeCacheMatrix returns a list of function closures for setting or getting
## the contents of a matrix and of the inverse of such matrix, both stored in
## the environment of the functions.
## The returned list behaves as a special matrix object with specific methods

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y) {
	x <<- y
	inverse <<- NULL
    }
    get <- function () x
    setinverse <- function (inv) inverse <<- inv
    getinverse <- function () inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve acts on objects returned by makeCacheMatrix and returns the
## inverse of the underlying matrix.
## The inverse matrix is actually computed and stored in the matrix object only
## if not already computed, otherwise it is simply retrieved from the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
	    message("getting cached inverse")
	    return(inv)
	}
	mtx <- x$get()
	inv <- solve(mtx, ...)
	x$setinverse(inv)
	inv
}

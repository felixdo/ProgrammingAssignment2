## A pair of functions that allow cached computation of inverse matrices
## Note: input matrices are assumed to be invertible

# Example Usage:
#
# > B <- matrix(c(2,2,3,2), ncol=2, nrow=2)
# > x <- makeCacheMatrix(B)
# cacheSolve(x)
#

# creates a cachematrix for an input matrix x. The result
# of this function can then be passed to cacheSolve() in order
# to compute the inverse of x such that repeated invocations
# use a cached result.
makeCacheMatrix <- function(x = matrix()) {

	# this is the cached result
	inverse <- NULL
	
	# set the input matrix (this can be used to reuse the vector returned
	# by makeCacheMatrix for a different input matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL #make sure to clear cached result
	}
	
	# returns the input matrix
	get <- function() x
	
	# set the inverse matrix.
	setinverse <- function(i) inverse <<- i
	getinverse <- function() inverse
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)

}


# returns the inverse of a matrix x. x
# must be a cachematrix obtained via makeCacheMatrix
cacheSolve <- function(x, ...) {
    
	# did we compute the result before?
	result <- x$getinverse()
	
	if (is.null(result)){
		# inverse has not been computed before, so compute it now
		matrix <- x$get() # this is the original input matrix
		result <- solve(matrix) # solve it
		x$setinverse(result) # cache the result in the cache matrix
	}

	result # return the result
}

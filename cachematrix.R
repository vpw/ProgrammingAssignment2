## Put comments here that give an overall description of what your
## functions do

## takes an invertible matrix as input and returns
## a wrapper object containing functions to process
## and store its values and inverse

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	## constructor
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	## get the underlying matrix object
	get <- function() x
	## set the inverse
	setInv <- function(inv) xinv <<- inv
	## get the cached inverse
	getInv <- function() xinv
	## list based access
	list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## takes a makdeCacheMatrix object and gets its inverse
## -> from cache if cached
## -> else with a call to solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	xinv <- x$getInv()
	## if xinv was computed
	if (!is.null(xinv)) {
		message("getting cached data")
		return(xinv)
	}
	#else
	data <- x$get()
	xinv <- solve(data, ...)
	x$setInv(xinv)
	xinv
}

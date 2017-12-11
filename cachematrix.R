## This code contains two functions:
## 1) makeCacheMatrix creates a special "matrix" object that can cache its
## inverse.
## 2) cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.

## makeCacheMatrix takes an invertible martix as input and returns an object x
## (the object is really a list) with the following methods:
## x$get() will return the matrix.
## x$set(y) will set new values for x according to y.
## x$setinv(inv) will cache inv as the inverse of x.
## x$getinv() will return the inverse of x, provided that one is defined
## (otherwise it will return a null value).

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set,
		get = get,
		setinv = setinv,
		getinv = getinv)
}


## cachSolve computes the inverse of a matrix created by makeCacheMatrix
## and caches it with that matrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv ## Return a matrix that is the inverse of 'x'
}

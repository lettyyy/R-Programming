## This function is going to cache the inverse of a matrix:
## Matrix inversion may be a costly computation and there may be some 
## benefit to caching the inverse rather than compute it everytime.
## So I would create a special object that can store the matrix and
## caches its inverse

## This function creates a "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## This function computes the inverse of the "matrix" created by
## makeCacheMatrix above, and if the inverse has already been 
## calculated, then the inverse should will be retrieved from the 
## cache, instead of being calculated again.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
		if (!is.null(inv)) {
				message("getting data from cache")
				return(inv)
		}
		mat <- x$get()
		inv <- solve(mat,...)
		x$setInverse(inv)
		inv
}

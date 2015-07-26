## -----------------------------------------------------------
## Programming Assignment 2
## Course - rprog-030
## -----------------------------------------------------------
## 
## This program provides 2 functions -

## makeCacheMatrix(matrix):
## 	This function accepts a matrix and wraps it within a list
##	of functions that works with parameter environment ('x')


makeCacheMatrix <- function(x = matrix()) {

	# cinv holds cached inverse value
	cinv <- NULL	# initial value is nil

	list (
		# define 'set' function: replaces the input matrix
		# data with new value. It also resets the cached
		# matrix inverse value (within 'x' env).
		set = function(y) {
			x <<- y
			cinv <<- NULL
		}
		,

		# define 'get' function: returns the matrix data
		get = function() x
		,

		# define 'setInv' function: saves the inverse matrix
		# value (within 'x' env)
		setInv = function(inv) cinv <<- inv
		,

		# define 'getInv' function: retrieves cached inverse 
		# matrix (from within 'x' env)
		getInv = function() cinv
	)
}


## cacheSolve(cacheMatrix): 
##	This method first checks if the inverse matrix is already
##	available in associated environment. If not then it solves
##	for the inverse, writes into cache.
##	It then returns the value.

cacheSolve <- function(x, ...) {

	# Retrieve the inverse matrix from cache
	inv <- x$getInv()

	if(is.null(inv)){

		# solve for inverse over data
		inv <- solve(x$get(),...)

		# save the inverse within 'x' env
		x$setInv(inv)
	}
	else {
		message("from cache")
	}

	# Return the inverse
	inv
}

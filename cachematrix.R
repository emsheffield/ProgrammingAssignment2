## These functions as a pair can be used to cache the inverse of a  
## matrix.

## The makeCacheMatrix function creates a "matrix" object that can 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function calculates the inverse of the matrix     
## created with the makeCacheMatrix function after checking to see if
## the inverse has already been calculated. If it has, it will      
## retrieve the cached information, and if not, it will calculate and 
## cache it. 

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

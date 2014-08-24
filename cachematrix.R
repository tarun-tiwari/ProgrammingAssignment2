## The makeCacheMatrix caches the input matrix and its initializes 
## the inverse matrix variable which is called matrix here. 
## It also defines 3 additional functions though which the next function 
## 'cacheSolve' sets the matrix value and accesses the cache for its reverse 
## and if the matrix value is not found already set that means that there is no pre-calculated value existing
## it then calculates the inverse matrix and puts in cache which can be accessed if called again for same matrix.

## The following functions initializes the matrix value and caches the inverse into variable
## called 'matrix'.

makeCacheMatrix <- function(x = matrix()) {
	matrix <- NULL
    set <- function(y) {
				x <<- y
				matrix <<- NULL
	}
	get <- function() x
	setmatrix <- function(inverse) matrix <<- inverse
	getmatrix <- function() matrix
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Following function looks for the existence of the matrix in the cache.
## If matrix is found in cache it returns the inverse matrix which is also in cache, 
## from the cache memory. If the input matrix does not exist in cache, it means that cache will
## not have its inverse too, then the program calculates the inverse of matrix and stores 
## the matrix and its inverse both in cache store by calling makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matrix <- x$getmatrix()
		if(!is.null(matrix)) {
			message("getting cached data for inverse matrix")
			return(matrix)
		}
		data <- x$get()
		matrix <- solve(data, ...)
		x$setmatrix(matrix)
		matrix
}

## This function caches the inverse of a matrix object

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## This part of the function retrieves the cached function.  If the matrix function has changed, then it computes the inverse.

cacheSolve <- function(x=matrix(), ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message('getting cached data')
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}

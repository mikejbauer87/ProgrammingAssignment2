
## The functions below caches the inverse of a matrix, rather than repeatedly
## computing. Utilizes 2 functions, one creating the special matrix
## and one that computes the inverse of the matrix

## The following function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	invrs <- NULL
	set <- function(y){
		x <<- y
		invrs <<- NULL
	}

	get <- function() x
	setinv <-function(solve) invrs <<- solve
	getinv <- function() invrs 
	list (set=set, get=get, 
	setinv=setinv,
	getinv=getinv)

}


## This function computes the inverse of the special matrix returned by the
## function makeCacheMatrix.  If inverse already calculated, cachesolve should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinv()
	if(!is.null(m)){
	message("getting cached data")
	return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinv(m)
	m

}


##  Getting the inverse of a matrix is typically a costly operation. 
##  It will take too long to compute the inversion especially if it
##  has to be computed repeatedly (e.g. in a loop). If the matrix is
##  not changing, it makes sense to cache the value of the inverse
##  so that when we need it again, it can be looked up in the cache
##  rather than recomputed. In this Programming Assignment we take
##  advantage of the scoping rules of the R language and how they
##  are manipulated to preserve state inside of an R object.

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to 
## 	1) set the matrix
##  2) get the matrix
##  3) set the inverse of matrix
##  4) get the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	setMtx <- function(y) {
		x <<- y
		m <<- NULL
	}
	getMtx <- function() x
	setInv <- function(inverse) m <<- inverse
	getInv <- function() m
	list(set = setMtx, get = getMtx,
		 setInverse = setInv, getInverse = getInv)
}

## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the inverse in the cache via the
## setinverse function.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting inversed matrix from cache...")
		return(m)
        }
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}

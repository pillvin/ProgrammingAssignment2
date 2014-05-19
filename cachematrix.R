##  Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse
#   get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

	xInv <- NULL;
	
	set <- function(y) {
		x <<- y;
		xInv <<- NULL;
	}
	
	get <- function() x;
	
	setInverse <- function(xdash) xInv <<- xdash;
	getInverse <- function() xInv;
	
	return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse));
}


## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix() function. 
#  However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
#  the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
#  of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		xInv <- x$getInverse();
		if(!is.null(xInv)) {
			message("getting cached data");
			return(xInv);
		}
		data <- x$get();
		xInv <- solve(data);
		x$setInverse(xInv);
		return(xInv);
}


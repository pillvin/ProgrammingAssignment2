##  Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the inverse
#   get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

	xInv <- NULL;
	
	#set matrix and reset mean to NULL.
	set <- function(y) {
		x <<- y;
		xInv <<- NULL;
	}
	
	#get matrix
	get <- function() x;
	
	#set inverse of the matrix
	setInverse <- function(xdash) xInv <<- xdash;
	
	#get inverse of the matrix
	getInverse <- function() xInv;
	
	#return list of methods. 
	return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse));
}


## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix() function. 
#  However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
#  the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
#  of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## get the inverse of x. It will be a non-NULL value if found in cache.
		xInv <- x$getInverse();
		
		#if inverse is not null, then return it.
		if(!is.null(xInv)) {
			message("getting cached data");
			return(xInv);
		}
		
		# if non found i.e. xInv==NULL, then compute inverse of x using solve() and cache it by 
		# invoking x$setInverse()
		data <- x$get();
		xInv <- solve(data);
		x$setInverse(xInv);
		
		#return inverse of x.
		return(xInv);
}


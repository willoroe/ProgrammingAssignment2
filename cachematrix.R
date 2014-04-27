## makeCacheMatrix sets up a list in the global environment that contains
## four functions that can be called from the global environment
## by $set, $get etc.
## cacheSolve finds and returns the inverse of a matrix if it has not already 
## been solved, if it has been solved it returns the cached value


makeCacheMatrix <- function(x = matrix()) {
	## Start off by making sure that the inverse is NULL
	inv <- NULL
	## When you use $set to set a new matrix set the inverse to NULL	
        set <- function(y) {
        	x <<- y
        	inv <<- NULL
            }
	## get is just a funtion that returns x        
        get <- function() x
	## setinv sets the inverse and sends it to the parent environment        
        setinv <- function(inverse) inv <<- inverse
	## getinv is a funtion that returns the value of inv
        getinv <- function() inv
	## put all those functions in a list in the parent environment
	list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first bit gets the value of inv
        inv <- x$getinv()
        ## if the matrix it gets is not null use that
        if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
    	## else, get the matrix and solve it and return inverse
    	data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
}

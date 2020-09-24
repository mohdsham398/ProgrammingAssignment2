## It creates a special matrix that can cache its inverse and it sets the values of the matrix and gets the value
## It contains a list to set and get the values of the matrix and also its inverse

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(i) i <- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The inverse of the special matrix is computed in this function
## This should retrieve the inverse from the cache if the inverse is already calculated

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached inverse")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


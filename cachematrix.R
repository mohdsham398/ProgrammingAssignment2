## This function creates a matrix that can cache the inverse of it 

## This function will create a special matrix and it sets and gets the 
## value of the matrix. Again the inverse operation is triggered and 
## it starts to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
		}
	get <- function()x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix. It retrieves
## the inverse of the cache if it is already been calculated

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
       

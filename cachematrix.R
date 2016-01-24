## Functions to compute matrix inversion
## using special object to cache inverse matrix

## This function creates an object that caches matrix inverse operation result

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix. The input object should be created using makeCacheMatrix and the matrix should be invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
    	message("using cached data")
    	return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## This creates two functions used to create a special object that stores a 
## matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. It
## is a list that contains functions to get and set the value of the matrix and
## get and set the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cachSolve calculates the inverse of the special "matrixr" created with the 
## makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
	if(!is.null(i)) {
	        message("getting cached data")
	        return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
        i       
}

##This R files contains two functions, which is useful in inversing matrix(es) and cache the result 


## The function "makeCacheMatrix" creates a list which could be referred to by the other function "CacheSolve"

makeCacheMatrix <- function(x = matrix()) {
	     i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse)i  <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This functino "CacheSolve" can inverse a matrix for the first time and return the inversed matrix to an object for the second time.

cacheSolve <- function(x, ...) {
	 i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        message("inverse is not in memory so the inverse (if exist) is gonna be computed")
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
	i
        ## Return a matrix that is the inverse of 'x'
}

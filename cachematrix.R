## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
	m <- NULL
	set <- function(y) {
        	x <<- y
	        m <<- NULL
	}
	get <- function() x
	setcacheSolve <- function(invmatrix) m <<- invmatrix
	getcacheSolve <- function() m
	list(set = set, get = get,
     		setInverse = setcacheSolve,
		     getInverse = getcacheSolve)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #compute inverse
        x$setInverse(m)
        m
}

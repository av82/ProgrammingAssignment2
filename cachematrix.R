## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makeCacheMatrix takes a matrix,provides a list of 
# functions set to set passed in matrix to local variable x 
 
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
#cacheSolve computes the invers of special matrix with attached functions set,get,setInverse,getInverse.
#if inverse is not null or already computed, simply returns it 
# else computes the inverse for data and sets its inverse using setInverse

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

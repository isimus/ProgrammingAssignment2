
#makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
	inverse <- NULL
	set <- function(y) {     #set the value of the matrix
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x     #get the value of the matrix
	setinverse <- function(solve) inverse <<- solve     #set the value of the inverse of the matrix
	getinverse <- function() inverse    				#get the value of the inverse of the matrix
	list(set = set, get = get, 
	setinverse = setinverse, 
	getinverse = getinverse)
}

#cacheSolve omputes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inverse <- x[getinverse()]
	if(!is.null(inverse)) {   		#See if the inverse does exist
			message("Getting cached matrix")
			return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...) 	#Compute the inverse
	
	x[setinverse(inverse)]
	inverse   						#Show the inverse

}


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #sets NULL as a default value to m
	  set <- function(y) {
	    	x <<- y ##assigns y to x
	    	m <<- NULL ##assigns a NULL value to m under the function of y
	}
	get <- function() x 
	setinverse <- function(inverse) m <<- inverse ##inverses m
	getinverse <- function() m ##prints the inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m<-x$getinverse()
		if(!is.null(m){ ##does following if m isnt null
		message("obtaining data that which has been cached")
		return(m)
		}
		data<-x$get()
		m<-solve(data%*%data) ##solves the matrix multiplication of the data
		x$setinverse(m) ##sets the inverse of m in terms of x
		m
}

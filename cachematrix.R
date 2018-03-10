## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix which is a list containing a function to the following:
        # Set the Value of matrix
        # get the value of matrix
        # Set the Matrix Inverse Value using setinverse
        # Get the Matrix Inverse Value using getinverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set value of a matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get value of a matrix
        get <- function() x
        # Set the Matrix Inverse Value using solve
        setinverse <- function(solve) m <<- solve
        # Get the Matrix Inverse Value
        getinverse <- function() m
        # creating list containing get,set,setinverse and getinverse functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cachesolve function computes the inverse of special matrix returned by makecachematrix
## It initially checks if the inverse has already been calculated. If yes, then gets the inverse from the cache rather than 
## performing complete computation. Else it calculates the inverse of the matrix and sets the value of the matrix inverse in cache
## using solve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # use catched data if inverse was already calculated for the matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # Calculate inverse if not available in cache 
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

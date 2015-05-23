## The first function creates a special "matrix" object 
## that can cache its inverse.
## The second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 


## This function creates a special "matrix" object 
## that can cache its inverse.
## It contains a list of functions that (1) set the matrix, 
## (2) get the matrix, (3) set the inverse of the matrix, and 
## (4) get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # set the matrix
        # plus restore the inverse to null
        
        get <- function() x
        # get the matrix
        
        setinv <- function(invert) inv <<- invert
        # set the inverse 
        
        getinv <- function() inv
        # get the value of the mean
        
        list(set=set, get=get,
             setinv = setinv,
             getinv = getinv)
        # store the function set, get, setinv and getinv in a list
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                # see if there is a previously defined 
                # invers matrix already
                return(inv)
                # if there is one, return that
        }
        
        data <- x$get()
        # get the matrix stored with makeCacheMatrix 
        # -> data to calculate the mean 
        
        inv <- solve(data, ...)
        # calculate the inverse of the matrix
        
        x$setinv(inv)
        # store the inverted matrix in the object "inv"
        
        inv
        # return inv        
}

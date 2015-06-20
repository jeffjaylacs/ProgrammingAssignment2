## The functions below provide a way to calculate the inverse of a matrix and
## store the result in cache memory.

## Function makeCacheMatrix accepts a matrix argument (the matrix supplied must be invertible) and
## returns a list of functions that can be used to save/retrieve its inverse to/from cache memory

makeCacheMatrix <- function(x = matrix()) {
    ## m is the matrix inverse.  Set the default value to NULL, indicating no inverse has been calculated
    m <- NULL
    
    ## This function can be used to over-write the matrix 'x' with some other matrix 'y'
    set <- function(y) {
        x <<- y
        m <<- NULL ## reset 'm' to NULL if the SET function is used to over-write matrix 'x'
    }    
    ## This function returns 'x' (the matrix input to the main function)
    get <- function() x 
    ## This function takes a matrix inverse and saves it to variable 'm'
    setinverse <- function(inverse) m <<- inverse
    ## This function returns the matrix inverse
    getinverse <- function() m
    
    ## return the list of functions created above so they can be called from other functions (e.g. cacheSolve)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##  The cacheSolve function computes and returns the inverse of the matrix 'x' that was input to makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function simply
##  retrieves the inverse from the cache rather than re-calculating.

cacheSolve <- function(x, ...) {
    ## Get the current value of the matrix inverse. It will either be NULL or valid value.
    m <- x$getinverse()
    ## if the inverse in cache memory is not NULL, use it!
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## else get the matrix and calculate its inverse.  Save it using the 'setinverse' function
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m ## return the inverse matrix
}
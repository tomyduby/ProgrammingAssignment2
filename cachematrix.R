## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the free variable m of this environment.
    m <- NULL

    ## This function initializes the x and m free variables in this environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## This function returns the value of x free variable in this envoronment
    get <- function() x
    
    ## This function sets the calculated inverse into free variable m
    ## in the environment of this function
    setinverse <- function(inverse) m <<- inverse
    
    ## This function returns the value of the free variable m
    getinverse <- function() m
        
    ## This is the last command in the makeCacheMatrix function. It is what
    ## this function returns: a list of four elements
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
    ## This function returns a matrix that is the inverse of 'x$get()'.
    ## It actually inverts the matrix x$get() only if it has not yet been
    ## calculated or the value of x$get() has changed.
    
    m <- x$getinverse()         # Get the value of the inverse matrix
    
    if( !is.null(m) ) {
        message("Getting cached matrix inverse")

        ## Check whether the value of matrix to be inverted has changed.
        #  Use R function identical to do that.
        if (identical(x$get(), x_old)) {
            return(m)
        }

        message("Matrix changed. Recalculating the inverse.")
    }


    ## Get the value of the matrix that needs to be inverted. And initialize
    ## the free variable x_old with the last matrix inverted
    x_old <<- x$get()

    ## Invert the matrix
    m <- solve(x_old)

    ## Save the inverted matrix value in the x list.
    x$setinverse(m)
    
    ## Return the result
    return(m)
}
